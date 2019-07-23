package coop.rchain.node

import java.nio.file.{Files, Path}

import cats._
import cats.effect._
import cats.effect.concurrent.Semaphore
import cats.implicits._
import cats.mtl.{ApplicativeLocal, DefaultApplicativeLocal}
import coop.rchain.blockstorage._
import coop.rchain.blockstorage.util.io.IOError
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.casper._
import coop.rchain.casper.engine.CasperLaunch.CasperInit
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.engine._
import coop.rchain.casper.util.comm.CasperPacketHandler
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import coop.rchain.comm.discovery._
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk, RPConfState}
import coop.rchain.comm.rp._
import coop.rchain.comm.transport._
import coop.rchain.grpc.Server
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.node.configuration.Configuration
import coop.rchain.node.diagnostics._
import coop.rchain.p2p.effects._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rspace.Context
import coop.rchain.shared.PathOps._
import coop.rchain.shared._
import kamon._
import kamon.system.SystemMetrics
import kamon.zipkin.ZipkinReporter
import monix.eval.{Task, TaskLocal}
import monix.execution.Scheduler
import org.http4s.server.blaze._

import scala.concurrent.duration._

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class NodeRuntime private[node] (
    conf: Configuration,
    id: NodeIdentifier,
    scheduler: Scheduler
)(
    implicit log: Log[Task],
    eventLog: EventLog[Task]
) {

  private[this] val loopScheduler =
    Scheduler.fixedPool("loop", 4, reporter = UncaughtExceptionLogger)
  private[this] val grpcScheduler =
    Scheduler.cached("grpc-io", 4, 64, reporter = UncaughtExceptionLogger)
  private[this] val availableProcessors = java.lang.Runtime.getRuntime.availableProcessors()
  // TODO: make it configurable
  // TODO: fine tune this
  private[this] val rspaceScheduler = Scheduler.forkJoin(
    name = "rspace",
    parallelism = availableProcessors * 2,
    maxThreads = availableProcessors * 2,
    reporter = UncaughtExceptionLogger
  )

  implicit private val logSource: LogSource = LogSource(this.getClass)

  /** Configuration */
  private val blockstorePath    = conf.server.dataDir.resolve("blockstore")
  private val dagStoragePath    = conf.server.dataDir.resolve("dagstorage")
  private val storagePath       = conf.server.dataDir.resolve("rspace")
  private val casperStoragePath = storagePath.resolve("casper")
  private val storageSize       = conf.server.mapSize
  private val defaultTimeout    = conf.server.defaultTimeout // TODO remove

  case class Servers(
      kademliaRPCServer: Server[Task],
      transportServer: TransportServer,
      externalApiServer: Server[Task],
      internalApiServer: Server[Task],
      httpServer: Fiber[Task, Unit]
  )

  def acquireServers(runtime: Runtime[Task], blockApiLock: Semaphore[Task])(
      implicit
      kademliaStore: KademliaStore[Task],
      nodeDiscovery: NodeDiscovery[Task],
      blockStore: BlockStore[Task],
      oracle: SafetyOracle[Task],
      multiParentCasperRef: MultiParentCasperRef[Task],
      connectionsCell: ConnectionsCell[Task],
      concurrent: Concurrent[Task],
      metrics: Metrics[Task],
      rPConfAsk: RPConfAsk[Task],
      span: Span[Task]
  ): Task[Servers] = {
    implicit val s: Scheduler = scheduler
    for {
      kademliaRPCServer <- discovery
                            .acquireKademliaRPCServer(
                              conf.server.networkId,
                              conf.server.kademliaPort,
                              KademliaHandleRPC.handlePing[Task],
                              KademliaHandleRPC.handleLookup[Task]
                            )(grpcScheduler)

      transportServer <- Task
                          .delay(
                            GrpcTransportServer.acquireServer(
                              conf.server.networkId,
                              conf.server.port,
                              conf.tls.certificate,
                              conf.tls.key,
                              conf.server.maxMessageSize,
                              conf.server.maxStreamMessageSize,
                              conf.server.dataDir.resolve("tmp").resolve("comm"),
                              conf.server.messageConsumers,
                              conf.kamon.zipkin
                            )(grpcScheduler, rPConfAsk, log, metrics)
                          )

      externalApiServer <- api
                            .acquireExternalServer[Task](
                              conf.grpcServer.portExternal,
                              grpcScheduler,
                              blockApiLock,
                              conf.kamon.zipkin
                            )
      internalApiServer <- api
                            .acquireInternalServer(
                              conf.grpcServer.portInternal,
                              runtime,
                              grpcScheduler,
                              blockApiLock,
                              conf.kamon.zipkin
                            )

      prometheusReporter = new NewPrometheusReporter()
      prometheusService  = NewPrometheusReporter.service[Task](prometheusReporter)

      httpServerFiber <- BlazeBuilder[Task]
                          .bindHttp(conf.server.httpPort, "0.0.0.0")
                          .mountService(prometheusService, "/metrics")
                          .mountService(VersionInfo.service[Task], "/version")
                          .mountService(StatusInfo.service[Task], "/status")
                          .resource
                          .use(_ => Task.never[Unit])
                          .start

      _ <- Task.delay {
            Kamon.reconfigure(conf.underlying.withFallback(Kamon.config()))
            if (conf.kamon.influxDb) Kamon.addReporter(new BatchInfluxDBReporter())
            if (conf.kamon.influxDbUdp) Kamon.addReporter(new UdpInfluxDBReporter())
            if (conf.kamon.prometheus) Kamon.addReporter(prometheusReporter)
            if (conf.kamon.zipkin) Kamon.addReporter(new ZipkinReporter())
            if (conf.kamon.sigar) SystemMetrics.startCollecting()
          }
    } yield Servers(
      kademliaRPCServer,
      transportServer,
      externalApiServer,
      internalApiServer,
      httpServerFiber
    )
  }

  def clearResources(servers: Servers, runtime: Runtime[Task], casperRuntime: Runtime[Task])(
      implicit
      blockStore: BlockStore[Task],
      blockDagStorage: BlockDagStorage[Task]
  ): Unit =
    (for {
      _ <- log.info("Shutting down API servers...")
      _ <- servers.externalApiServer.stop
      _ <- servers.internalApiServer.stop
      _ <- log.info("Shutting down Kademlia RPC server...")
      _ <- servers.kademliaRPCServer.stop
      _ <- log.info("Shutting down transport layer...")
      _ <- servers.transportServer.stop()
      _ <- log.info("Shutting down HTTP server....")
      _ <- Task.delay(Kamon.stopAllReporters())
      _ <- servers.httpServer.cancel
      _ <- log.info("Shutting down interpreter runtime ...")
      _ <- runtime.close()
      _ <- log.info("Shutting down Casper runtime ...")
      _ <- casperRuntime.close()
      _ <- log.info("Bringing DagStorage down ...")
      _ <- blockDagStorage.close()
      _ <- log.info("Bringing BlockStore down ...")
      _ <- blockStore.close()
      _ <- log.info("Goodbye.")
    } yield ()).unsafeRunSyncTracing(conf.kamon.zipkin)(scheduler)

  def addShutdownHook(servers: Servers, runtime: Runtime[Task], casperRuntime: Runtime[Task])(
      implicit blockStore: BlockStore[Task],
      blockDagStorage: BlockDagStorage[Task]
  ): Task[Unit] =
    Task.delay(sys.addShutdownHook(clearResources(servers, runtime, casperRuntime)))

  private def exit0: Task[Unit] = Task.delay(System.exit(0))

  private def nodeProgram(
      runtime: Runtime[Task],
      casperRuntime: Runtime[Task]
  )(
      implicit
      time: Time[Task],
      rpConfState: RPConfState[Task],
      rpConfAsk: RPConfAsk[Task],
      peerNodeAsk: PeerNodeAsk[Task],
      metrics: Metrics[Task],
      span: Span[Task],
      transport: TransportLayer[Task],
      kademliaStore: KademliaStore[Task],
      nodeDiscovery: NodeDiscovery[Task],
      rpConnectons: ConnectionsCell[Task],
      blockDagStorage: BlockDagStorage[Task],
      blockStore: BlockStore[Task],
      oracle: SafetyOracle[Task],
      packetHandler: PacketHandler[Task],
      casperConstructor: MultiParentCasperRef[Task],
      engineCell: EngineCell[Task],
      requestedBlocks: Running.RequestedBlocks[Task]
  ): Task[Unit] = {

    val info: Task[Unit] =
      if (conf.server.standalone) Log[Task].info(s"Starting stand-alone node.")
      else Log[Task].info(s"Starting node that will bootstrap from ${conf.server.bootstrap}")

    val dynamicIpCheck: Task[Unit] =
      if (conf.server.dynamicHostAddress)
        for {
          local <- peerNodeAsk.ask
          newLocal <- WhoAmI
                       .checkLocalPeerNode[Task](conf.server.port, conf.server.kademliaPort, local)
          _ <- newLocal.fold(Task.unit) { pn =>
                Connect.resetConnections[Task].flatMap(kp(rpConfState.modify(_.copy(local = pn))))
              }
        } yield ()
      else Task.unit

    val nodeDiscoveryLoop: Task[Unit] =
      for {
        _ <- NodeDiscovery[Task].discover
        _ <- Connect.findAndConnect[Task](Connect.connect[Task])
        _ <- time.sleep(20.seconds)
      } yield ()

    val clearConnectionsLoop: Task[Unit] =
      for {
        _ <- dynamicIpCheck
        _ <- Connect.clearConnections[Task]
        _ <- time.sleep(10.minutes)
      } yield ()

    def waitForFirstConnetion: Task[Unit] =
      for {
        _ <- time.sleep(10.second)
        _ <- ConnectionsCell[Task].read.map(_.isEmpty).ifM(waitForFirstConnetion, ().pure[Task])
      } yield ()

    val casperLoop: Task[Unit] =
      for {
        casper <- casperConstructor.get
        _      <- casper.fold(().pure[Task])(_.fetchDependencies)
        _      <- Running.maintainRequestedBlocks[Task]
        _      <- time.sleep(30.seconds)
      } yield ()

    for {
      blockApiLock <- Semaphore[Task](1)
      _            <- info
      local        <- peerNodeAsk.ask
      host         = local.endpoint.host
      servers      <- acquireServers(runtime, blockApiLock)
      _            <- addShutdownHook(servers, runtime, casperRuntime)
      _            <- servers.externalApiServer.start
      _ <- Log[Task].info(
            s"External API server started at $host:${servers.externalApiServer.port}"
          )
      _ <- servers.internalApiServer.start
      _ <- Log[Task].info(
            s"Internal API server started at $host:${servers.internalApiServer.port}"
          )
      _ <- servers.kademliaRPCServer.start
      _ <- Log[Task].info(
            s"Kademlia RPC server started at $host:${servers.kademliaRPCServer.port}"
          )
      _ <- servers.transportServer.start(
            pm => HandleMessages.handle[Task](pm),
            blob => packetHandler.handlePacket(blob.sender, blob.packet)
          )
      address = local.toAddress
      _       <- Log[Task].info(s"Listening for traffic on $address.")
      _       <- EventLog[Task].publish(Event.NodeStarted(address))
      _       <- Task.defer(nodeDiscoveryLoop.forever).executeOn(loopScheduler).start
      _       <- Task.defer(clearConnectionsLoop.forever).executeOn(loopScheduler).start
      _ <- if (conf.server.standalone) ().pure[Task]
          else Log[Task].info(s"Waiting for first connection.") >> waitForFirstConnetion
      _ <- Concurrent[Task].start(EngineCell[Task].read >>= (_.init))
      _ <- Task.defer(casperLoop.forever).executeOn(loopScheduler)
    } yield ()
  }

  /**
    * Handles unrecoverable errors in program. Those are errors that should not happen in properly
    * configured enviornment and they mean immediate termination of the program
    */
  private def handleUnrecoverableErrors(prog: Task[Unit]): Task[Unit] =
    prog
      .onErrorHandleWith { th =>
        log.error("Caught unhandable error. Exiting. Stacktrace below.") >> Task.delay {
          th.printStackTrace()
        }
      } >> exit0.as(Right(()))

  private def localScope(source: Metrics.Source): Task[ApplicativeLocal[Task, Trace]] =
    TaskLocal[Trace](Trace.source(source))
      .map { ls =>
        new DefaultApplicativeLocal[Task, Trace] {
          val tl: TaskLocal[Trace] = ls

          override def local[A](f: Trace => Trace)(fa: Task[A]): Task[A] =
            tl.read.flatMap(t => tl.bind(f(t))(fa))

          override val applicative: Applicative[Task] = Applicative[Task]

          override def ask: Task[Trace] = tl.read
        }
      }

  private def setupSpan(enabled: Boolean, networkId: String, host: String): Task[Span[Task]] =
    if (!enabled) Task.now(Span.noop[Task])
    else
      localScope(Metrics.BaseSource)
        .map(implicit ls => diagnostics.effects.span[Task](networkId, host))

  private val rpClearConnConf = ClearConnectionsConf(
    numOfConnectionsPinged = 10
  ) // TODO read from conf

  private def rpConf(local: PeerNode, bootstrapNode: Option[PeerNode]) =
    RPConf(
      local,
      conf.server.networkId,
      bootstrapNode,
      defaultTimeout,
      conf.server.maxNumOfConnections,
      rpClearConnConf
    )

  // TODO this should use existing algebra
  private def mkDirs(path: Path): Task[Unit] =
    Sync[Task].delay(Files.createDirectories(path))

  /**
    * Main node entry. It will:
    * 1. set up configurations
    * 2. create instances of typeclasses
    * 3. run the node program.
    */
  // TODO: Resolve scheduler chaos in Runtime, RuntimeManager and CasperPacketHandler
  val main: Task[Unit] = for {
    // 1. fetch local peer node
    local <- WhoAmI
              .fetchLocalPeerNode[Task](
                conf.server.host,
                conf.server.port,
                conf.server.kademliaPort,
                conf.server.noUpnp,
                id
              )

    // 2. set up configurations
    defaultTimeout = conf.server.defaultTimeout

    // 3. create instances of typeclasses
    initPeer             = if (conf.server.standalone) None else Some(conf.server.bootstrap)
    rpConfState          = effects.rpConfState(rpConf(local, initPeer))
    rpConfAsk            = effects.rpConfAsk(rpConfState)
    peerNodeAsk          = effects.peerNodeAsk(rpConfState)
    rpConnections        <- effects.rpConnections
    metrics              = diagnostics.effects.metrics[Task]
    span                 <- setupSpan(conf.kamon.zipkin, conf.server.networkId, local.endpoint.host)
    time                 = effects.time
    timerTask            = Task.timer
    multiParentCasperRef <- MultiParentCasperRef.of[Task]
    lab                  <- LastApprovedBlock.of[Task]
    commTmpFolder        = conf.server.dataDir.resolve("tmp").resolve("comm")
    _ <- commTmpFolder.toFile
          .exists()
          .fold(
            commTmpFolder.deleteDirectory[Task](),
            Task.unit
          )
    transport <- effects
                  .transportClient(
                    conf.server.networkId,
                    conf.tls.certificate,
                    conf.tls.key,
                    conf.server.maxMessageSize,
                    conf.server.packetChunkSize,
                    commTmpFolder
                  )(grpcScheduler, log, metrics)

    kademliaRPC = effects.kademliaRPC(
      conf.server.networkId,
      defaultTimeout,
      conf.server.allowPrivateAddresses
    )(
      grpcScheduler,
      peerNodeAsk,
      metrics
    )
    kademliaStore = effects.kademliaStore(id)(kademliaRPC, metrics)
    _             <- initPeer.fold(Task.unit)(p => kademliaStore.updateLastSeen(p))
    nodeDiscovery = effects.nodeDiscovery(id)(kademliaStore, kademliaRPC)

    /**
      * We need to come up with a consistent way with folder creation. Some layers create folder on their own (if not available),
      * others (like blockstore) relay on the structure being created for them (and will fail if it does not exist). For now
      * this small fix should suffice, but we should unify this.
      */
    _             <- mkDirs(conf.server.dataDir)
    _             <- mkDirs(blockstorePath)
    _             <- mkDirs(dagStoragePath)
    blockstoreEnv = Context.env(blockstorePath, 8L * 1024L * 1024L * 1024L)
    blockStore <- FileLMDBIndexBlockStore
                   .create[Task](blockstoreEnv, blockstorePath)(
                     Concurrent[Task],
                     Sync[Task],
                     log
                   )
                   .map(_.right.get) // TODO handle errors
    dagConfig = BlockDagFileStorage.Config(
      latestMessagesLogPath = dagStoragePath.resolve("latestMessagesLogPath"),
      latestMessagesCrcPath = dagStoragePath.resolve("latestMessagesCrcPath"),
      blockMetadataLogPath = dagStoragePath.resolve("blockMetadataLogPath"),
      blockMetadataCrcPath = dagStoragePath.resolve("blockMetadataCrcPath"),
      equivocationsTrackerLogPath = dagStoragePath.resolve("equivocationsTrackerLogPath"),
      equivocationsTrackerCrcPath = dagStoragePath.resolve("equivocationsTrackerCrcPath"),
      invalidBlocksLogPath = dagStoragePath.resolve("invalidBlocksLogPath"),
      invalidBlocksCrcPath = dagStoragePath.resolve("invalidBlocksCrcPath"),
      checkpointsDirPath = dagStoragePath.resolve("checkpointsDirPath"),
      blockNumberIndexPath = dagStoragePath.resolve("blockNumberIndexPath"),
      mapSize = 8L * 1024L * 1024L * 1024L,
      latestMessagesLogMaxSizeFactor = 10
    )
    blockDagStorage <- BlockDagFileStorage.create[Task](dagConfig)(
                        Concurrent[Task],
                        Sync[Task],
                        log
                      )
    oracle = SafetyOracle
      .cliqueOracle[Task](
        Monad[Task],
        log,
        metrics,
        span
      )
    lastFinalizedBlockCalculator = LastFinalizedBlockCalculator[Task](
      conf.server.faultToleranceThreshold
    )(
      Sync[Task],
      log,
      Concurrent[Task],
      blockStore,
      blockDagStorage,
      oracle
    )
    runtime <- {
      implicit val s                = rspaceScheduler
      implicit val m: Metrics[Task] = metrics
      implicit val sp: Span[Task]   = span
      Runtime
        .createWithEmptyCost[Task](storagePath, storageSize, Seq.empty)
    }
    _ <- {
      implicit val sp: Span[Task] = span
      Runtime
        .injectEmptyRegistryRoot[Task](runtime.space, runtime.replaySpace)
    }

    casperRuntime <- {
      implicit val s                = rspaceScheduler
      implicit val m: Metrics[Task] = metrics
      implicit val sp: Span[Task]   = span
      Runtime
        .createWithEmptyCost[Task](
          casperStoragePath,
          storageSize,
          Seq.empty
        )

    }
    runtimeManager <- {
      implicit val m: Metrics[Task] = metrics
      implicit val sp: Span[Task]   = span
      RuntimeManager.fromRuntime[Task](casperRuntime)
    }
    engineCell      <- EngineCell.init[Task]
    envVars         = EnvVars.envVars[Task]
    raiseIOError    = IOError.raiseIOErrorThroughSync[Task]
    requestedBlocks <- Cell.mvarCell[Task, Map[BlockHash, Running.Requested]](Map.empty)
    casperInit = new CasperInit[Task](
      conf.casper,
      conf.kamon.zipkin
    )
    _ <- CasperLaunch[Task](casperInit, identity)(
          lab,
          metrics,
          span,
          blockStore,
          rpConnections,
          nodeDiscovery,
          transport,
          rpConfAsk,
          oracle,
          lastFinalizedBlockCalculator,
          Sync[Task],
          Concurrent[Task],
          time,
          log,
          eventLog,
          multiParentCasperRef,
          blockDagStorage,
          engineCell,
          envVars,
          raiseIOError,
          runtimeManager,
          requestedBlocks,
          scheduler
        )
    packetHandler = {
      implicit val ev: EngineCell[Task] = engineCell
      CasperPacketHandler[Task]
    }

    // 4. run the node program.
    program = nodeProgram(runtime, casperRuntime)(
      time,
      rpConfState,
      rpConfAsk,
      peerNodeAsk,
      metrics,
      span,
      transport,
      kademliaStore,
      nodeDiscovery,
      rpConnections,
      blockDagStorage,
      blockStore,
      oracle,
      packetHandler,
      multiParentCasperRef,
      engineCell,
      requestedBlocks
    )
    _ <- handleUnrecoverableErrors(program)
  } yield ()

}

object NodeRuntime {
  def apply(
      conf: Configuration
  )(implicit scheduler: Scheduler, log: Log[Task], eventLog: EventLog[Task]): Task[NodeRuntime] =
    for {
      id      <- NodeEnvironment.create(conf)
      runtime <- Task.delay(new NodeRuntime(conf, id, scheduler))
    } yield runtime
}
