package coop.rchain.node

import java.nio.ByteBuffer
import java.nio.file.{Files, Path}

import cats._
import cats.effect._
import cats.effect.concurrent.Semaphore
import cats.implicits._
import cats.mtl.{ApplicativeAsk, MonadState}
import cats.temp.par.Par
import coop.rchain.blockstorage._
import coop.rchain.blockstorage.util.io.IOError
import coop.rchain.casper._
import coop.rchain.casper.engine.CasperLaunch.CasperInit
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.engine._
import coop.rchain.casper.protocol.{DeployServiceGrpcMonix, ProposeServiceGrpcMonix}
import coop.rchain.casper.util.comm.CasperPacketHandler
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib.Taskable
import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import coop.rchain.comm.discovery._
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk, RPConfState}
import coop.rchain.comm.rp._
import coop.rchain.comm.transport._
import coop.rchain.grpc.Server
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.node.NodeRuntime.{APIServers, CasperLoop, Cleanup, EngineInit, RuntimeConf}
import coop.rchain.node.api.{DeployGrpcService, ProposeGrpcService, ReplGrpcService}
import coop.rchain.node.configuration.Configuration
import coop.rchain.node.diagnostics._
import coop.rchain.node.model.repl.ReplGrpcMonix
import coop.rchain.p2p.effects._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rspace.Context
import coop.rchain.shared.PathOps._
import coop.rchain.shared._
import kamon._
import kamon.system.SystemMetrics
import kamon.zipkin.ZipkinReporter
import monix.eval.Task
import monix.execution.Scheduler
import org.http4s.implicits._
import org.http4s.server.blaze._
import org.http4s.server.Router
import org.lmdbjava.Env

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
  private[this] val rspaceScheduler         = RChainScheduler.interpreterScheduler
  implicit private val logSource: LogSource = LogSource(this.getClass)

  /** Configuration */
  private val blockstorePath    = conf.server.dataDir.resolve("blockstore")
  private val dagStoragePath    = conf.server.dataDir.resolve("dagstorage")
  private val storagePath       = conf.server.dataDir.resolve("rspace")
  private val casperStoragePath = storagePath.resolve("casper")
  private val storageSize       = conf.server.mapSize
  private val defaultTimeout    = conf.server.defaultTimeout // TODO remove

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

    // 3. create instances of typeclasses
    metrics       = diagnostics.effects.metrics[Task]
    time          = effects.time[Task]
    commTmpFolder = conf.server.dataDir.resolve("tmp").resolve("comm")
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

    initPeer       = if (conf.server.standalone) None else Some(conf.server.bootstrap)
    peerNode       = rpConf(local, initPeer)
    rpConfState    = effects.rpConfState[Task](peerNode)
    peerNodeAsk    = effects.peerNodeAsk[Task](Monad[Task], Sync[Task], rpConfState)
    defaultTimeout = conf.server.defaultTimeout
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
    nodeDiscovery = effects.nodeDiscovery(id)(Monad[Task], kademliaStore, kademliaRPC)

    /**
      * We need to come up with a consistent way with folder creation. Some layers create folder on their own (if not available),
      * others (like blockstore) relay on the structure being created for them (and will fail if it does not exist). For now
      * this small fix should suffice, but we should unify this.
      */
    _             <- mkDirs(conf.server.dataDir)
    _             <- mkDirs(blockstorePath)
    _             <- mkDirs(dagStoragePath)
    blockstoreEnv = Context.env(blockstorePath, 8L * 1024L * 1024L * 1024L)
    dagConfig = BlockDagFileStorage.Config(
      latestMessagesLogPath = dagStoragePath.resolve("latestMessagesLogPath"),
      latestMessagesCrcPath = dagStoragePath.resolve("latestMessagesCrcPath"),
      blockMetadataLogPath = dagStoragePath.resolve("blockMetadataLogPath"),
      blockMetadataCrcPath = dagStoragePath.resolve("blockMetadataCrcPath"),
      equivocationsTrackerLogPath = dagStoragePath.resolve("equivocationsTrackerLogPath"),
      equivocationsTrackerCrcPath = dagStoragePath.resolve("equivocationsTrackerCrcPath"),
      invalidBlocksLogPath = dagStoragePath.resolve("invalidBlocksLogPath"),
      invalidBlocksCrcPath = dagStoragePath.resolve("invalidBlocksCrcPath"),
      blockHashesByDeployLogPath = dagStoragePath.resolve("blockHashesByDeployLogPath"),
      blockHashesByDeployCrcPath = dagStoragePath.resolve("blockHashesByDeployCrcPath"),
      checkpointsDirPath = dagStoragePath.resolve("checkpointsDirPath"),
      blockNumberIndexPath = dagStoragePath.resolve("blockNumberIndexPath"),
      mapSize = 8L * 1024L * 1024L * 1024L,
      latestMessagesLogMaxSizeFactor = 10
    )
    casperConfig = RuntimeConf(casperStoragePath, storageSize)
    cliConfig    = RuntimeConf(storagePath, 800L * 1024L * 1024L) // 800MB for cli

    result <- NodeRuntime.setupNodeProgramF[Task](
               rpConfState,
               conf,
               dagConfig,
               blockstoreEnv,
               casperConfig,
               cliConfig,
               blockstorePath,
               rspaceScheduler,
               scheduler
             )(
               metrics,
               transport,
               Sync[Task],
               Concurrent[Task],
               time,
               log,
               eventLog,
               ContextShift[Task],
               Par[Task],
               Taskable[Task]
             )
    (
      rpConnections,
      rpConfAsk,
      blockStore,
      blockDagStorage,
      runtimeCleanup,
      packetHandler,
      apiServers,
      casperLoop,
      engineInit
    ) = result

    // 4. run the node program.
    program = nodeProgram(apiServers, casperLoop, engineInit, runtimeCleanup)(
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
      packetHandler
    )
    _ <- handleUnrecoverableErrors(program)
  } yield ()

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

  private def nodeProgram(
      apiServers: APIServers,
      casperLoop: CasperLoop[Task],
      engineInit: EngineInit[Task],
      runtimeCleanup: Cleanup[Task]
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
      rpConnections: ConnectionsCell[Task],
      blockDagStorage: BlockDagStorage[Task],
      blockStore: BlockStore[Task],
      packetHandler: PacketHandler[Task]
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

    def waitForFirstConnection: Task[Unit] =
      for {
        _ <- time.sleep(10.second)
        _ <- ConnectionsCell[Task].read.map(_.isEmpty).ifM(waitForFirstConnection, ().pure[Task])
      } yield ()

    for {
      _     <- info
      local <- peerNodeAsk.ask
      host  = local.endpoint.host
      servers <- acquireServers(apiServers)(
                  kademliaStore,
                  nodeDiscovery,
                  rpConnections,
                  Concurrent[Task],
                  metrics,
                  rpConfAsk
                )
      _ <- addShutdownHook(servers, runtimeCleanup)
      _ <- servers.externalApiServer.start
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
          else Log[Task].info(s"Waiting for first connection.") >> waitForFirstConnection
      _ <- Concurrent[Task].start(engineInit)
      _ <- Task.defer(casperLoop.forever).executeOn(loopScheduler)
    } yield ()
  }

  def addShutdownHook(
      servers: Servers,
      runtimeCleanup: Cleanup[Task]
  )(
      implicit blockStore: BlockStore[Task],
      blockDagStorage: BlockDagStorage[Task]
  ): Task[Unit] =
    Task.delay(sys.addShutdownHook(clearResources(servers, runtimeCleanup)))

  def clearResources(
      servers: Servers,
      runtimeCleanup: Cleanup[Task]
  )(
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
      _ <- servers.httpServer.cancel.attempt
      _ <- runtimeCleanup.close()
      _ <- log.info("Bringing DagStorage down ...")
      _ <- blockDagStorage.close()
      _ <- log.info("Bringing BlockStore down ...")
      _ <- blockStore.close()
      _ <- log.info("Goodbye.")
    } yield ()).unsafeRunSync(scheduler)

  private def exit0: Task[Unit] = Task.delay(System.exit(0))

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

  case class Servers(
      kademliaRPCServer: Server[Task],
      transportServer: TransportServer,
      externalApiServer: Server[Task],
      internalApiServer: Server[Task],
      httpServer: Fiber[Task, Unit]
  )

  def acquireServers(apiServers: APIServers)(
      implicit
      kademliaStore: KademliaStore[Task],
      nodeDiscovery: NodeDiscovery[Task],
      connectionsCell: ConnectionsCell[Task],
      concurrent: Concurrent[Task],
      metrics: Metrics[Task],
      rPConfAsk: RPConfAsk[Task]
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
                              conf.server.messageConsumers
                            )(grpcScheduler, rPConfAsk, log, metrics)
                          )

      externalApiServer <- api
                            .acquireExternalServer[Task](
                              conf.grpcServer.portExternal,
                              grpcScheduler,
                              apiServers.deploy,
                              apiServers.propose
                            )
      internalApiServer <- api
                            .acquireInternalServer(
                              conf.grpcServer.portInternal,
                              grpcScheduler,
                              apiServers.repl,
                              apiServers.propose
                            )

      prometheusReporter = new NewPrometheusReporter()
      prometheusService  = NewPrometheusReporter.service[Task](prometheusReporter)

      httpServerFiber <- BlazeServerBuilder[Task]
                          .bindHttp(conf.server.httpPort, "0.0.0.0")
                          .withHttpApp(
                            Router(
                              "/metrics" -> prometheusService,
                              "/version" -> VersionInfo.service[Task],
                              "/status"  -> StatusInfo.service[Task]
                            ).orNotFound
                          )
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

}

object NodeRuntime {

  type CasperLoop[F[_]] = F[Unit]
  type EngineInit[F[_]] = F[Unit]

  final case class RuntimeConf(
      storage: Path,
      size: Long
  )

  def apply(
      conf: Configuration
  )(implicit scheduler: Scheduler, log: Log[Task], eventLog: EventLog[Task]): Task[NodeRuntime] =
    for {
      id      <- NodeEnvironment.create(conf)
      runtime <- Task.delay(new NodeRuntime(conf, id, scheduler))
    } yield runtime

  trait Cleanup[F[_]] {
    def close(): F[Unit]
  }

  def cleanup[F[_]: Sync: Log](runtime: Runtime[F], casperRuntime: Runtime[F]): Cleanup[F] =
    new Cleanup[F] {
      override def close(): F[Unit] =
        for {
          _ <- Log[F].info("Shutting down interpreter runtime ...")
          _ <- runtime.close()
          _ <- Log[F].info("Shutting down Casper runtime ...")
          _ <- casperRuntime.close()
        } yield ()
    }

  def setupNodeProgramF[F[_]: Metrics: TransportLayer: Sync: Concurrent: Time: Log: EventLog: ContextShift: Par: Taskable](
      rpConfState: MonadState[F, RPConf],
      conf: Configuration,
      dagConfig: BlockDagFileStorage.Config,
      blockstoreEnv: Env[ByteBuffer],
      casperConf: RuntimeConf,
      cliConf: RuntimeConf,
      blockstorePath: Path,
      rspaceScheduler: Scheduler,
      scheduler: Scheduler
  ): F[
    (
        ConnectionsCell[F],
        ApplicativeAsk[F, RPConf],
        BlockStore[F],
        BlockDagFileStorage[F],
        Cleanup[F],
        PacketHandler[F],
        APIServers,
        CasperLoop[F],
        EngineInit[F]
    )
  ] =
    for {
      rpConnections <- effects.rpConnections
      rpConfAsk     = effects.rpConfAsk(Monad[F], Sync[F], rpConfState)
      lab           <- LastApprovedBlock.of[F]
      blockStore <- FileLMDBIndexBlockStore
                     .create[F](blockstoreEnv, blockstorePath)(
                       Concurrent[F],
                       Sync[F],
                       Log[F],
                       Metrics[F]
                     )
                     .map(_.right.get) // TODO handle errors
      span            = Span.noop[F]
      blockDagStorage <- BlockDagFileStorage.create[F](dagConfig)
      oracle = {
        implicit val sp = span
        SafetyOracle.cliqueOracle[F]
      }
      lastFinalizedBlockCalculator = LastFinalizedBlockCalculator[F](
        conf.server.faultToleranceThreshold
      )(
        Sync[F],
        Log[F],
        Concurrent[F],
        blockStore,
        blockDagStorage,
        oracle
      )
      runtime <- {
        implicit val s  = rspaceScheduler
        implicit val sp = span
        Runtime
          .createWithEmptyCost[F](cliConf.storage, cliConf.size, Seq.empty)
      }
      _ <- Runtime.bootstrapRegistry[F](runtime)
      casperRuntime <- {
        implicit val s  = rspaceScheduler
        implicit val sp = span
        Runtime
          .createWithEmptyCost[F](
            casperConf.storage,
            casperConf.size,
            Seq.empty
          )
      }
      runtimeManager <- {
        implicit val sp = span
        RuntimeManager.fromRuntime[F](casperRuntime)
      }
      engineCell      <- EngineCell.init[F]
      envVars         = EnvVars.envVars[F]
      raiseIOError    = IOError.raiseIOErrorThroughSync[F]
      requestedBlocks <- Cell.mvarCell[F, Map[BlockHash, Running.Requested]](Map.empty)
      casperInit      = new CasperInit[F](conf.casper)
      _ <- {
        implicit val bs = blockStore
        implicit val bd = blockDagStorage
        implicit val ec = engineCell
        implicit val ev = envVars
        implicit val re = raiseIOError
        implicit val rb = requestedBlocks
        implicit val rm = runtimeManager
        implicit val or = oracle
        implicit val lc = lastFinalizedBlockCalculator
        implicit val sp = span
        implicit val lb = lab
        implicit val rc = rpConnections
        implicit val ra = rpConfAsk
        CasperLaunch[F](casperInit)
      }
      packetHandler = {
        implicit val ev: EngineCell[F] = engineCell
        CasperPacketHandler[F]
      }
      blockApiLock <- Semaphore[F](1)
      apiServers = NodeRuntime.acquireAPIServers[F](runtime, blockApiLock, scheduler)(
        blockStore,
        oracle,
        Concurrent[F],
        span,
        engineCell,
        Log[F],
        Taskable[F]
      )
      casperLoop = for {
        engine <- engineCell.read
        _      <- engine.withCasper(_.fetchDependencies, engine.noop)
        _ <- Running.maintainRequestedBlocks[F](
              Monad[F],
              rpConfAsk,
              requestedBlocks,
              TransportLayer[F],
              Log[F],
              Time[F]
            )
        _ <- Time[F].sleep(30.seconds)
      } yield ()
      engineInit     = engineCell.read >>= (_.init)
      runtimeCleanup = NodeRuntime.cleanup(runtime, casperRuntime)
    } yield (
      rpConnections,
      rpConfAsk,
      blockStore,
      blockDagStorage,
      runtimeCleanup,
      packetHandler,
      apiServers,
      casperLoop,
      engineInit
    )

  final case class APIServers(
      repl: ReplGrpcMonix.Repl,
      propose: ProposeServiceGrpcMonix.ProposeService,
      deploy: DeployServiceGrpcMonix.DeployService
  )

  def acquireAPIServers[F[_]](
      runtime: Runtime[F],
      blockApiLock: Semaphore[F],
      scheduler: Scheduler
  )(
      implicit
      blockStore: BlockStore[F],
      oracle: SafetyOracle[F],
      concurrent: Concurrent[F],
      span: Span[F],
      engineCell: EngineCell[F],
      logF: Log[F],
      taskable: Taskable[F]
  ): APIServers = {
    implicit val s: Scheduler = scheduler
    val repl                  = ReplGrpcService.instance(runtime, s)
    val deploy                = DeployGrpcService.instance(blockApiLock)
    val propose               = ProposeGrpcService.instance(blockApiLock)
    APIServers(repl, propose, deploy)
  }
}
