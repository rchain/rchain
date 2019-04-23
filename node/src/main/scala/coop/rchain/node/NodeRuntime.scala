package coop.rchain.node

import java.nio.file.{Files, Path}

import scala.concurrent.duration._

import cats._
import cats.data._
import cats.effect._
import cats.effect.concurrent.Semaphore
import cats.implicits._

import coop.rchain.blockstorage._
import coop.rchain.casper._
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.casper.util.comm.{CasperInit, CasperPacketHandler}
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm.discovery._
import coop.rchain.comm.rp._
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk, RPConfState}
import coop.rchain.comm.transport._
import coop.rchain.grpc.Server
import coop.rchain.metrics.Metrics
import coop.rchain.node.configuration.Configuration
import coop.rchain.node.diagnostics._
import coop.rchain.p2p.effects._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rspace.Context
import coop.rchain.shared._
import coop.rchain.shared.PathOps._

import kamon._
import kamon.system.SystemMetrics
import kamon.zipkin.ZipkinReporter
import monix.eval.Task
import monix.execution.Scheduler
import org.http4s.server.blaze._

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class NodeRuntime private[node] (
    conf: Configuration,
    id: NodeIdentifier,
    scheduler: Scheduler
)(implicit log: Log[Task]) {

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

  implicit def eiterTrpConfAsk(implicit ev: RPConfAsk[Task]): RPConfAsk[Effect] =
    new EitherTApplicativeAsk[Task, RPConf, CommError]

  import ApplicativeError_._

  /** Configuration */
  private val port              = conf.server.port
  private val kademliaPort      = conf.server.kademliaPort
  private val blockstorePath    = conf.server.dataDir.resolve("blockstore")
  private val dagStoragePath    = conf.server.dataDir.resolve("dagstorage")
  private val storagePath       = conf.server.dataDir.resolve("rspace")
  private val casperStoragePath = storagePath.resolve("casper")
  private val storageSize       = conf.server.mapSize
  private val storeType         = conf.server.storeType
  private val defaultTimeout    = conf.server.defaultTimeout // TODO remove

  case class Servers(
      kademliaRPCServer: Server[Task],
      transportServer: TransportServer,
      externalApiServer: Server[Effect],
      internalApiServer: Server[Task],
      httpServer: Fiber[Task, Unit]
  )

  def acquireServers(runtime: Runtime[Task], blockApiLock: Semaphore[Effect])(
      implicit
      kademliaStore: KademliaStore[Task],
      nodeDiscovery: NodeDiscovery[Task],
      blockStore: BlockStore[Effect],
      oracle: SafetyOracle[Effect],
      multiParentCasperRef: MultiParentCasperRef[Effect],
      nodeCoreMetrics: NodeMetrics[Task],
      jvmMetrics: JvmMetrics[Task],
      connectionsCell: ConnectionsCell[Task],
      concurrent: Concurrent[Effect],
      metrics: Metrics[Task],
      rPConfAsk: RPConfAsk[Task]
  ): Effect[Servers] = {
    implicit val s: Scheduler = scheduler
    for {
      kademliaRPCServer <- discovery
                            .acquireKademliaRPCServer(
                              conf.server.networkId,
                              kademliaPort,
                              KademliaHandleRPC.handlePing[Task],
                              KademliaHandleRPC.handleLookup[Task]
                            )(grpcScheduler)
                            .toEffect
      transportServer <- Task
                          .delay(
                            GrpcTransportServer.acquireServer(
                              conf.server.networkId,
                              conf.server.port,
                              conf.tls.certificate,
                              conf.tls.key,
                              conf.grpcServer.maxMessageSize,
                              conf.server.dataDir.resolve("tmp").resolve("comm"),
                              conf.server.messageConsumers
                            )(grpcScheduler, rPConfAsk, log)
                          )
                          .toEffect
      externalApiServer <- api
                            .acquireExternalServer[Effect](
                              conf.grpcServer.portExternal,
                              grpcScheduler,
                              blockApiLock
                            )
      internalApiServer <- api
                            .acquireInternalServer(
                              conf.grpcServer.portInternal,
                              runtime,
                              grpcScheduler
                            )
                            .toEffect

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
                          .toEffect

      _ <- Task.delay {
            Kamon.reconfigure(conf.underlying.withFallback(Kamon.config()))
            if (conf.kamon.influxDb) Kamon.addReporter(new BatchInfluxDBReporter())
            if (conf.kamon.influxDbUdp) Kamon.addReporter(new UdpInfluxDBReporter())
            if (conf.kamon.prometheus) Kamon.addReporter(prometheusReporter)
            if (conf.kamon.zipkin) Kamon.addReporter(new ZipkinReporter())
            Kamon.addReporter(new JmxReporter())
            if (conf.kamon.sigar) SystemMetrics.startCollecting()
          }.toEffect
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
      blockStore: BlockStore[Effect],
      blockDagStorage: BlockDagStorage[Effect]
  ): Unit =
    (for {
      _ <- log.info("Shutting down API servers...")
      _ <- servers.externalApiServer.stop.value
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
      _ <- blockDagStorage.close().value
      _ <- log.info("Bringing BlockStore down ...")
      _ <- blockStore.close().value
      _ <- log.info("Goodbye.")
    } yield ()).unsafeRunSync(scheduler)

  def addShutdownHook(servers: Servers, runtime: Runtime[Task], casperRuntime: Runtime[Task])(
      implicit blockStore: BlockStore[Effect],
      blockDagStorage: BlockDagStorage[Effect]
  ): Task[Unit] =
    Task.delay(sys.addShutdownHook(clearResources(servers, runtime, casperRuntime)))

  private def exit0: Task[Unit] = Task.delay(System.exit(0))

  private def nodeProgram(
      runtime: Runtime[Task],
      casperRuntime: Runtime[Task],
      casperPacketHandler: CasperPacketHandler[Effect]
  )(
      implicit
      time: Time[Task],
      rpConfState: RPConfState[Task],
      rpConfAsk: RPConfAsk[Task],
      peerNodeAsk: PeerNodeAsk[Task],
      metrics: Metrics[Task],
      transport: TransportLayer[Task],
      kademliaStore: KademliaStore[Task],
      nodeDiscovery: NodeDiscovery[Task],
      rpConnectons: ConnectionsCell[Task],
      blockDagStorage: BlockDagStorage[Effect],
      blockStore: BlockStore[Effect],
      oracle: SafetyOracle[Effect],
      packetHandler: PacketHandler[Effect],
      casperConstructor: MultiParentCasperRef[Effect],
      nodeCoreMetrics: NodeMetrics[Task],
      jvmMetrics: JvmMetrics[Task]
  ): Effect[Unit] = {

    val info: Effect[Unit] =
      if (conf.server.standalone) Log[Effect].info(s"Starting stand-alone node.")
      else Log[Effect].info(s"Starting node that will bootstrap from ${conf.server.bootstrap}")

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

    val loop: Effect[Unit] =
      for {
        _ <- dynamicIpCheck.toEffect
        _ <- NodeDiscovery[Effect].discover
        _ <- Connect.clearConnections[Effect]
        _ <- Connect.findAndConnect[Effect](Connect.connect[Effect])
        _ <- time.sleep(20.seconds).toEffect
      } yield ()

    def waitForFirstConnetion: Effect[Unit] =
      for {
        _ <- time.sleep(10.second).toEffect
        _ <- ConnectionsCell[Effect].read.map(_.isEmpty).ifM(waitForFirstConnetion, ().pure[Effect])
      } yield ()

    val casperLoop: Effect[Unit] =
      for {
        casper <- casperConstructor.get
        _      <- casper.fold(().pure[Effect])(_.fetchDependencies)
        _      <- time.sleep(30.seconds).toEffect
      } yield ()

    for {
      blockApiLock <- Semaphore[Effect](1)
      _            <- info
      local        <- peerNodeAsk.ask.toEffect
      host         = local.endpoint.host
      servers      <- acquireServers(runtime, blockApiLock)
      _            <- addShutdownHook(servers, runtime, casperRuntime).toEffect
      _            <- servers.externalApiServer.start
      _ <- Log[Effect].info(
            s"External API server started at $host:${servers.externalApiServer.port}"
          )
      _ <- servers.internalApiServer.start.toEffect
      _ <- Log[Effect].info(
            s"Internal API server started at $host:${servers.internalApiServer.port}"
          )
      _ <- servers.kademliaRPCServer.start.toEffect
      _ <- Log[Effect].info(
            s"Kademlia RPC server started at $host:${servers.kademliaRPCServer.port}"
          )
      _ <- servers.transportServer.startWithEffects(
            pm => HandleMessages.handle[Effect](pm),
            blob => packetHandler.handlePacket(blob.sender, blob.packet).as(())
          )
      address = s"rnode://$id@$host?protocol=$port&discovery=$kademliaPort"
      _       <- Log[Effect].info(s"Listening for traffic on $address.")
      _       <- Task.defer(loop.forever.value).executeOn(loopScheduler).start.toEffect
      _ <- if (conf.server.standalone) ().pure[Effect]
          else Log[Effect].info(s"Waiting for first connection.") >> waitForFirstConnetion
      _ <- Concurrent[Effect].start(casperPacketHandler.init)
      _ <- Task.defer(casperLoop.forever.value).executeOn(loopScheduler).toEffect
    } yield ()
  }

  /**
    * Handles unrecoverable errors in program. Those are errors that should not happen in properly
    * configured enviornment and they mean immediate termination of the program
    */
  private def handleUnrecoverableErrors(prog: Effect[Unit]): Effect[Unit] =
    EitherT[Task, CommError, Unit](
      prog.value
        .onErrorHandleWith { th =>
          log.error("Caught unhandable error. Exiting. Stacktrace below.") *> Task.delay {
            th.printStackTrace()
          }
        } *> exit0.as(Right(()))
    )

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
  private def mkDirs(path: Path): Effect[Unit] =
    Sync[Effect].delay(Files.createDirectories(path))

  /**
    * Main node entry. It will:
    * 1. set up configurations
    * 2. create instances of typeclasses
    * 3. run the node program.
    */
  // TODO: Resolve scheduler chaos in Runtime, RuntimeManager and CasperPacketHandler
  val main: Effect[Unit] = for {
    // 1. fetch local peer node
    local <- WhoAmI
              .fetchLocalPeerNode[Task](
                conf.server.host,
                conf.server.port,
                conf.server.kademliaPort,
                conf.server.noUpnp,
                id
              )
              .toEffect

    // 2. set up configurations
    defaultTimeout = conf.server.defaultTimeout

    // 3. create instances of typeclasses
    initPeer             = if (conf.server.standalone) None else Some(conf.server.bootstrap)
    rpConfState          = effects.rpConfState(rpConf(local, initPeer))
    rpConfAsk            = effects.rpConfAsk(rpConfState)
    peerNodeAsk          = effects.peerNodeAsk(rpConfState)
    rpConnections        <- effects.rpConnections.toEffect
    metrics              = diagnostics.effects.metrics[Task]
    time                 = effects.time
    timerTask            = Task.timer
    multiParentCasperRef <- MultiParentCasperRef.of[Effect]
    lab                  <- LastApprovedBlock.of[Task].toEffect
    labEff               = LastApprovedBlock.eitherTLastApprovedBlock[CommError, Task](Monad[Task], lab)
    commTmpFolder        = conf.server.dataDir.resolve("tmp").resolve("comm")
    _ <- commTmpFolder.toFile
          .exists()
          .fold(
            commTmpFolder.deleteDirectory[Task]().toEffect,
            Task.unit.toEffect
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
                  .toEffect
    kademliaRPC = effects.kademliaRPC(conf.server.networkId, defaultTimeout)(
      grpcScheduler,
      peerNodeAsk,
      metrics
    )
    kademliaStore = effects.kademliaStore(id)(kademliaRPC, metrics)
    _             <- initPeer.fold(Task.unit.toEffect)(p => kademliaStore.updateLastSeen(p).toEffect)
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
                   .create[Effect](blockstoreEnv, blockstorePath)(
                     Concurrent[Effect],
                     Sync[Effect],
                     Log.eitherTLog(Monad[Task], log)
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
    blockDagStorage <- BlockDagFileStorage.create[Effect](dagConfig)(
                        Concurrent[Effect],
                        Sync[Effect],
                        Log.eitherTLog(Monad[Task], log)
                      )
    oracle = SafetyOracle
      .cliqueOracle[Effect](
        Monad[Effect],
        Log.eitherTLog(Monad[Task], log),
        Metrics.eitherT(Monad[Task], metrics)
      )
    runtime <- {
      implicit val s                = rspaceScheduler
      implicit val m: Metrics[Task] = metrics
      Runtime
        .createWithEmptyCost[Task, Task.Par](storagePath, storageSize, storeType, Seq.empty)
        .toEffect
    }
    _ <- Runtime
          .injectEmptyRegistryRoot[Task](runtime.space, runtime.replaySpace)
          .toEffect
    casperRuntime <- {
      implicit val s                = rspaceScheduler
      implicit val m: Metrics[Task] = metrics
      Runtime
        .createWithEmptyCost[Task, Task.Par](
          casperStoragePath,
          storageSize,
          storeType,
          Seq.empty
        )
        .toEffect
    }
    runtimeManager <- RuntimeManager.fromRuntime[Task](casperRuntime).toEffect
    casperPacketHandler <- CasperPacketHandler
                            .of[Effect](
                              new CasperInit(
                                conf.casper,
                                defaultTimeout,
                                RuntimeManager.eitherTRuntimeManager(runtimeManager)
                              ),
                              _.value
                            )(
                              labEff,
                              Metrics.eitherT(Monad[Task], metrics),
                              blockStore,
                              Cell.eitherTCell(Monad[Task], rpConnections),
                              NodeDiscovery.eitherTNodeDiscovery(Monad[Task], nodeDiscovery),
                              TransportLayer.eitherTTransportLayer(Monad[Task], log, transport),
                              ErrorHandler[Effect],
                              eiterTrpConfAsk(rpConfAsk),
                              oracle,
                              Sync[Effect],
                              Concurrent[Effect],
                              Time.eitherTTime(Monad[Task], time),
                              Log.eitherTLog(Monad[Task], log),
                              multiParentCasperRef,
                              blockDagStorage,
                              scheduler
                            )
    packetHandler = PacketHandler.pf[Effect](casperPacketHandler.handle)(
      Applicative[Effect],
      Log.eitherTLog(Monad[Task], log),
      ErrorHandler[Effect]
    )
    nodeCoreMetrics = diagnostics.effects.nodeCoreMetrics[Task]
    jvmMetrics      = diagnostics.effects.jvmMetrics[Task]

    // 4. run the node program.
    program = nodeProgram(runtime, casperRuntime, casperPacketHandler)(
      time,
      rpConfState,
      rpConfAsk,
      peerNodeAsk,
      metrics,
      transport,
      kademliaStore,
      nodeDiscovery,
      rpConnections,
      blockDagStorage,
      blockStore,
      oracle,
      packetHandler,
      multiParentCasperRef,
      nodeCoreMetrics,
      jvmMetrics
    )
    _ <- handleUnrecoverableErrors(program)
  } yield ()

}

object NodeRuntime {
  def apply(
      conf: Configuration
  )(implicit scheduler: Scheduler, log: Log[Task]): Effect[NodeRuntime] =
    for {
      id      <- NodeEnvironment.create(conf)
      runtime <- Task.delay(new NodeRuntime(conf, id, scheduler)).toEffect
    } yield runtime
}
