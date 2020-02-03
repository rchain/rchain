package coop.rchain.node

import java.nio.ByteBuffer
import java.nio.file.{Files, Path}

import scala.concurrent.duration._
import cats._
import cats.data.ReaderT
import cats.effect._
import cats.effect.concurrent.Semaphore
import cats.implicits._
import cats.mtl._
import cats.tagless.implicits._
import coop.rchain.blockstorage._
import coop.rchain.blockstorage.dag.{BlockDagFileStorage, BlockDagStorage}
import coop.rchain.blockstorage.deploy.{InMemDeployStorage, LMDBDeployStorage}
import coop.rchain.blockstorage.finality.LastFinalizedFileStorage
import coop.rchain.blockstorage.util.io.IOError
import coop.rchain.casper._
import coop.rchain.casper.engine._
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.engine.Running.Requested
import coop.rchain.casper.protocol.deploy.v1.DeployServiceV1GrpcMonix
import coop.rchain.casper.protocol.propose.v1.ProposeServiceV1GrpcMonix
import coop.rchain.casper.util.comm._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.Taskable
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import coop.rchain.comm.discovery._
import coop.rchain.comm.rp._
import coop.rchain.comm.rp.Connect.{Connections, ConnectionsCell, RPConfAsk, RPConfState}
import coop.rchain.comm.transport._
import coop.rchain.grpc.Server
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.node.NodeRuntime.{apply => _, _}
import coop.rchain.node.api.WebApi.WebApiImpl
import coop.rchain.node.api._
import coop.rchain.node.configuration.Configuration
import coop.rchain.node.diagnostics._
import coop.rchain.node.diagnostics.Trace.TraceId
import coop.rchain.node.effects.{EventConsumer, RchainEvents}
import coop.rchain.node.model.repl.ReplGrpcMonix
import coop.rchain.node.web._
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
import org.lmdbjava.Env

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
  private val blockstorePath           = conf.server.dataDir.resolve("blockstore")
  private val dagStoragePath           = conf.server.dataDir.resolve("dagstorage")
  private val deployStoragePath        = conf.server.dataDir.resolve("deploystorage")
  private val lastFinalizedStoragePath = conf.server.dataDir.resolve("last-finalized-block")
  private val storagePath              = conf.server.dataDir.resolve("rspace")
  private val casperStoragePath        = storagePath.resolve("casper")
  private val storageSize              = conf.server.mapSize
  private val defaultTimeout           = conf.server.defaultTimeout // TODO remove

  implicit class RichTask[A](t: Task[A]) {
    def toReaderT: TaskEnv[A] =
      ReaderT.liftF(t)
  }

  /**
    * Main node entry. It will:
    * 1. set up configurations
    * 2. create instances of typeclasses
    * 3. run the node program.
    */
  // TODO: Resolve scheduler chaos in Runtime, RuntimeManager and CasperPacketHandler
  val main: TaskEnv[Unit] = for {
    // 1. fetch local peer node
    local <- WhoAmI
              .fetchLocalPeerNode[Task](
                conf.server.host,
                conf.server.port,
                conf.server.kademliaPort,
                conf.server.noUpnp,
                id
              )
              .toReaderT

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
          .toReaderT
    transport <- effects
                  .transportClient(
                    conf.server.networkId,
                    conf.tls.certificate,
                    conf.tls.key,
                    conf.server.maxMessageSize,
                    conf.server.packetChunkSize,
                    commTmpFolder
                  )(grpcScheduler, log, metrics)
                  .toReaderT
    rpConnections   <- effects.rpConnections[Task].toReaderT
    initPeer        = if (conf.server.standalone) None else Some(conf.server.bootstrap)
    peerNode        = rpConf(local, initPeer)
    rpConfState     = effects.rpConfState[Task](peerNode)
    peerNodeAsk     = effects.peerNodeAsk[Task](Monad[Task], Sync[Task], rpConfState)
    rpConfAsk       = effects.rpConfAsk[Task](Monad[Task], Sync[Task], rpConfState)
    requestedBlocks <- Cell.mvarCell[Task, Map[BlockHash, Running.Requested]](Map.empty).toReaderT
    commUtil = CommUtil.of[Task](
      Concurrent[Task],
      log,
      time,
      metrics,
      transport,
      rpConnections,
      rpConfAsk,
      requestedBlocks
    )

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
    _             <- initPeer.fold(Task.unit)(p => kademliaStore.updateLastSeen(p)).toReaderT
    nodeDiscovery = effects.nodeDiscovery(id)(Monad[Task], kademliaStore, kademliaRPC)

    /**
      * We need to come up with a consistent way with folder creation. Some layers create folder on their own (if not available),
      * others (like blockstore) relay on the structure being created for them (and will fail if it does not exist). For now
      * this small fix should suffice, but we should unify this.
      */
    _             <- mkDirs(conf.server.dataDir).toReaderT
    _             <- mkDirs(blockstorePath).toReaderT
    _             <- mkDirs(dagStoragePath).toReaderT
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
    deployStorageConfig = LMDBDeployStorage.Config(
      storagePath = deployStoragePath,
      mapSize = 1024L * 1024L * 1024L
    )
    casperConfig = RuntimeConf(casperStoragePath, storageSize)
    cliConfig    = RuntimeConf(storagePath, 800L * 1024L * 1024L) // 800MB for cli

    rpConfAskEnv = effects.readerTApplicativeAsk[Task, NodeCallCtx, RPConf](rpConfAsk)
    rpConfStateEnv = effects.readerTMonadState[Task, NodeCallCtx, RPConf](
      rpConfState
    )
    peerNodeAskEnv = effects.readerTApplicativeAsk[Task, NodeCallCtx, PeerNode](
      peerNodeAsk
    )
    metricsEnv         = Metrics.readerTMetrics[Task, NodeCallCtx](metrics)
    transportEnv       = transport.mapK(taskToEnv)
    timeEnv            = time.mapK(taskToEnv)
    logEnv             = log.mapK(taskToEnv)
    eventLogEnv        = eventLog.mapK(taskToEnv)
    nodeDiscoveryEnv   = nodeDiscovery.mapK(taskToEnv)
    rpConnectionsEnv   = Cell.readerT[Task, NodeCallCtx, Connections](rpConnections)
    requestedBlocksEnv = Cell.readerT[Task, NodeCallCtx, Map[BlockHash, Requested]](requestedBlocks)
    commUtilEnv        = commUtil.mapK(taskToEnv)
    taskableEnv = new Taskable[TaskEnv] {
      override def toTask[A](fa: TaskEnv[A]): Task[A] = fa.run(NodeCallCtx.init)
    }
    localEnvironment = cats.mtl.instances.all.localReader[Task, NodeCallCtx]
    eventBus         <- RchainEvents.readerTInstance[Task, NodeCallCtx]
    result <- NodeRuntime.setupNodeProgramF[TaskEnv](
               rpConnectionsEnv,
               rpConfAskEnv,
               rpConfStateEnv,
               commUtilEnv,
               requestedBlocksEnv,
               conf,
               dagConfig,
               blockstoreEnv,
               casperConfig,
               cliConfig,
               blockstorePath,
               lastFinalizedStoragePath,
               rspaceScheduler,
               scheduler,
               eventBus,
               deployStorageConfig
             )(
               metricsEnv,
               transportEnv,
               Sync[TaskEnv],
               Concurrent[TaskEnv],
               timeEnv,
               logEnv,
               eventLogEnv,
               ContextShift[TaskEnv],
               Parallel[TaskEnv],
               taskableEnv,
               localEnvironment
             )
    (
      blockStore,
      blockDagStorage,
      runtimeCleanup,
      packetHandler,
      apiServers,
      casperLoop,
      engineInit,
      casperLaunch,
      reportingCasper,
      webApi
    ) = result

    // 4. launch casper
    _ <- casperLaunch.launch()

    // 5. run the node program.
    program = nodeProgram(
      apiServers,
      casperLoop,
      engineInit,
      runtimeCleanup,
      reportingCasper,
      webApi
    )(
      logEnv,
      timeEnv,
      rpConfStateEnv,
      rpConfAsk,
      rpConfAskEnv,
      peerNodeAskEnv,
      metrics,
      metricsEnv,
      transportEnv,
      kademliaStore,
      nodeDiscovery,
      nodeDiscoveryEnv,
      rpConnections,
      rpConnectionsEnv,
      blockDagStorage,
      blockStore,
      packetHandler,
      eventLogEnv,
      eventBus
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
      casperLoop: CasperLoop[TaskEnv],
      engineInit: EngineInit[TaskEnv],
      runtimeCleanup: Cleanup[TaskEnv],
      reportingCasper: ReportingCasper[TaskEnv],
      webApi: WebApi[TaskEnv]
  )(
      implicit
      logEnv: Log[TaskEnv],
      timeEnv: Time[TaskEnv],
      rpConfState: RPConfState[TaskEnv],
      rpConfAsk: RPConfAsk[Task],
      rpConfAskEnv: RPConfAsk[TaskEnv],
      peerNodeAsk: PeerNodeAsk[TaskEnv],
      metrics: Metrics[Task],
      metricsEnv: Metrics[TaskEnv],
      transport: TransportLayer[TaskEnv],
      kademliaStore: KademliaStore[Task],
      nodeDiscoveryTask: NodeDiscovery[Task],
      nodeDiscovery: NodeDiscovery[TaskEnv],
      rpConnections: ConnectionsCell[Task],
      rpConnectionsEnv: ConnectionsCell[TaskEnv],
      blockDagStorage: BlockDagStorage[TaskEnv],
      blockStore: BlockStore[TaskEnv],
      packetHandler: PacketHandler[TaskEnv],
      eventLog: EventLog[TaskEnv],
      consumer: EventConsumer[Task]
  ): TaskEnv[Unit] = {

    val info: TaskEnv[Unit] =
      if (conf.server.standalone) Log[TaskEnv].info(s"Starting stand-alone node.")
      else Log[TaskEnv].info(s"Starting node that will bootstrap from ${conf.server.bootstrap}")

    val dynamicIpCheck: TaskEnv[Unit] =
      if (conf.server.dynamicHostAddress)
        for {
          local <- peerNodeAsk.ask
          newLocal <- WhoAmI
                       .checkLocalPeerNode[TaskEnv](
                         conf.server.port,
                         conf.server.kademliaPort,
                         local
                       )
          _ <- newLocal.fold(Task.unit.toReaderT) { pn =>
                Connect
                  .resetConnections[TaskEnv]
                  .flatMap(kp(rpConfState.modify(_.copy(local = pn))))
              }
        } yield ()
      else Task.unit.toReaderT

    val nodeDiscoveryLoop: TaskEnv[Unit] =
      for {
        _ <- NodeDiscovery[TaskEnv].discover
        _ <- Connect.findAndConnect[TaskEnv](Connect.connect[TaskEnv])
        _ <- timeEnv.sleep(20.seconds)
      } yield ()

    val clearConnectionsLoop: TaskEnv[Unit] =
      for {
        _ <- dynamicIpCheck
        _ <- Connect.clearConnections[TaskEnv]
        _ <- timeEnv.sleep(10.minutes)
      } yield ()

    def waitForFirstConnection: TaskEnv[Unit] =
      for {
        _ <- timeEnv.sleep(10.second)
        _ <- ConnectionsCell[TaskEnv].read
              .map(_.isEmpty)
              .ifM(waitForFirstConnection, ().pure[TaskEnv])
      } yield ()

    for {
      _     <- info
      local <- peerNodeAsk.ask
      host  = local.endpoint.host
      servers <- acquireServers(apiServers, reportingCasper, webApi)(
                  kademliaStore,
                  nodeDiscoveryTask,
                  rpConnections,
                  Concurrent[Task],
                  metrics,
                  rpConfAsk,
                  consumer
                ).toReaderT
      _ <- addShutdownHook(servers, runtimeCleanup)
      _ <- servers.externalApiServer.start.toReaderT

      _ <- Log[TaskEnv].info(
            s"External API server started at $host:${servers.externalApiServer.port}"
          )
      _ <- servers.internalApiServer.start.toReaderT

      _ <- Log[TaskEnv].info(
            s"Internal API server started at $host:${servers.internalApiServer.port}"
          )
      _ <- servers.kademliaRPCServer.start.toReaderT

      // HTTP server is started immediately on `acquireServers`
      _ <- Log[TaskEnv].info(
            s"HTTP API server started at $host:${conf.server.httpPort}"
          )

      _ <- Log[TaskEnv].info(
            s"Kademlia RPC server started at $host:${servers.kademliaRPCServer.port}"
          )
      _ <- servers.transportServer
            .start(
              pm => HandleMessages.handle[TaskEnv](pm).run(NodeCallCtx.init),
              blob =>
                packetHandler
                  .handlePacket(blob.sender, blob.packet)
                  .run(NodeCallCtx.init)
            )
            .toReaderT
      address = local.toAddress
      _       <- Log[TaskEnv].info(s"Listening for traffic on $address.")
      _       <- EventLog[TaskEnv].publish(Event.NodeStarted(address))
      _ <- Task
            .defer(nodeDiscoveryLoop.forever.run(NodeCallCtx.init))
            .executeOn(loopScheduler)
            .start
            .toReaderT
      _ <- Task
            .defer(clearConnectionsLoop.forever.run(NodeCallCtx.init))
            .executeOn(loopScheduler)
            .start
            .toReaderT
      _ <- if (conf.server.standalone) ().pure[TaskEnv]
          else Log[TaskEnv].info(s"Waiting for first connection.") >> waitForFirstConnection
      _ <- Concurrent[TaskEnv].start(engineInit)
      _ <- Task
            .defer(casperLoop.forever.run(NodeCallCtx.init))
            .executeOn(loopScheduler)
            .toReaderT
    } yield ()
  }

  def addShutdownHook(
      servers: Servers,
      runtimeCleanup: Cleanup[TaskEnv]
  )(
      implicit blockStore: BlockStore[TaskEnv],
      blockDagStorage: BlockDagStorage[TaskEnv],
      log: Log[TaskEnv]
  ): TaskEnv[Unit] =
    Task.delay(sys.addShutdownHook(clearResources(servers, runtimeCleanup))).as(()).toReaderT

  def clearResources(
      servers: Servers,
      runtimeCleanup: Cleanup[TaskEnv]
  )(
      implicit
      blockStore: BlockStore[TaskEnv],
      blockDagStorage: BlockDagStorage[TaskEnv],
      log: Log[TaskEnv]
  ): Unit =
    (for {
      _ <- Log[TaskEnv].info("Shutting down API servers...")
      _ <- servers.externalApiServer.stop.toReaderT
      _ <- servers.internalApiServer.stop.toReaderT
      _ <- Log[TaskEnv].info("Shutting down Kademlia RPC server...")
      _ <- servers.kademliaRPCServer.stop.toReaderT
      _ <- Log[TaskEnv].info("Shutting down transport layer...")
      _ <- servers.transportServer.stop().toReaderT
      _ <- Log[TaskEnv].info("Shutting down HTTP server....")
      _ <- Task.delay(Kamon.stopAllReporters()).toReaderT
      _ <- servers.httpServer.cancel.attempt.toReaderT
      _ <- runtimeCleanup.close()
      _ <- Log[TaskEnv].info("Bringing DagStorage down ...")
      _ <- blockDagStorage.close()
      _ <- Log[TaskEnv].info("Bringing BlockStore down ...")
      _ <- blockStore.close()
      _ <- Log[TaskEnv].info("Goodbye.")
    } yield ()).run(NodeCallCtx.init).unsafeRunSync(scheduler)

  private def exit0: Task[Unit] = Task.delay(System.exit(0))

  /**
    * Handles unrecoverable errors in program. Those are errors that should not happen in properly
    * configured enviornment and they mean immediate termination of the program
    */
  private def handleUnrecoverableErrors(prog: TaskEnv[Unit]): TaskEnv[Unit] =
    prog.mapF(_.onErrorHandleWith { th =>
      log.error("Caught unhandable error. Exiting. Stacktrace below.") >> Task.delay {
        th.printStackTrace()
      }
    } >> exit0.as(Right(())))

  case class Servers(
      kademliaRPCServer: Server[Task],
      transportServer: TransportServer,
      externalApiServer: Server[Task],
      internalApiServer: Server[Task],
      httpServer: Fiber[Task, Unit]
  )

  def acquireServers(
      apiServers: APIServers,
      reportingCasper: ReportingCasper[TaskEnv],
      webApi: WebApi[TaskEnv]
  )(
      implicit
      kademliaStore: KademliaStore[Task],
      nodeDiscovery: NodeDiscovery[Task],
      connectionsCell: ConnectionsCell[Task],
      concurrent: Concurrent[Task],
      metrics: Metrics[Task],
      rPConfAsk: RPConfAsk[Task],
      consumer: EventConsumer[Task]
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
      httpServerFiber = aquireHttpServer(
        conf.server.reporting,
        conf.server.httpPort,
        prometheusReporter,
        reportingCasper,
        webApi
      )(nodeDiscovery, connectionsCell, concurrent, rPConfAsk, consumer, s)
      httpFiber <- httpServerFiber.start
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
      httpFiber
    )
  }
}

final case class NodeCallCtx(trace: TraceId) {
  def next: NodeCallCtx = this.copy(trace = Trace.next)
}
object NodeCallCtx {
  def init: NodeCallCtx = NodeCallCtx(Trace.next)
}

object NodeRuntime {
  type TaskEnv[A]             = ReaderT[Task, NodeCallCtx, A]
  type LocalEnvironment[F[_]] = ApplicativeLocal[F, NodeCallCtx]

  val taskToEnv: Task ~> TaskEnv = λ[Task ~> TaskEnv](ReaderT.liftF(_))
  val envToTask: TaskEnv ~> Task = λ[TaskEnv ~> Task](_.run(NodeCallCtx.init))

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

  def cleanup[F[_]: Sync: Log](
      runtime: Runtime[F],
      casperRuntime: Runtime[F],
      deployStorageCleanup: F[Unit]
  ): Cleanup[F] =
    new Cleanup[F] {
      override def close(): F[Unit] =
        for {
          _ <- Log[F].info("Shutting down interpreter runtime ...")
          _ <- runtime.close()
          _ <- Log[F].info("Shutting down Casper runtime ...")
          _ <- casperRuntime.close()
          _ <- Log[F].info("Shutting down deploy storage ...")
          _ <- deployStorageCleanup
        } yield ()
    }

  def setupNodeProgramF[F[_]: Metrics: TransportLayer: Sync: Concurrent: Time: Log: EventLog: ContextShift: Parallel: Taskable: LocalEnvironment](
      rpConnections: ConnectionsCell[F],
      rpConfAsk: ApplicativeAsk[F, RPConf],
      rpConfState: MonadState[F, RPConf],
      commUtil: CommUtil[F],
      requestedBlocks: Running.RequestedBlocks[F],
      conf: Configuration,
      dagConfig: BlockDagFileStorage.Config,
      blockstoreEnv: Env[ByteBuffer],
      casperConf: RuntimeConf,
      cliConf: RuntimeConf,
      blockstorePath: Path,
      lastFinalizedPath: Path,
      rspaceScheduler: Scheduler,
      scheduler: Scheduler,
      eventPublisher: EventPublisher[F],
      deployStorageConfig: LMDBDeployStorage.Config
  ): F[
    (
        BlockStore[F],
        BlockDagFileStorage[F],
        Cleanup[F],
        PacketHandler[F],
        APIServers,
        CasperLoop[F],
        EngineInit[F],
        CasperLaunch[F],
        ReportingCasper[F],
        WebApi[F]
    )
  ] =
    for {
      lab <- LastApprovedBlock.of[F]
      blockStore <- FileLMDBIndexBlockStore
                     .create[F](blockstoreEnv, blockstorePath)(
                       Concurrent[F],
                       Sync[F],
                       Log[F],
                       Metrics[F]
                     )
                     .map(_.right.get) // TODO handle errors
      span = if (conf.kamon.zipkin)
        diagnostics.effects.span(conf.server.networkId, conf.server.host.getOrElse("-"))
      else Span.noop[F]
      blockDagStorage                       <- BlockDagFileStorage.create[F](dagConfig)
      lastFinalizedStorage                  <- LastFinalizedFileStorage.make[F](lastFinalizedPath)
      deployStorageAllocation               <- LMDBDeployStorage.make[F](deployStorageConfig).allocated
      (deployStorage, deployStorageCleanup) = deployStorageAllocation
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
        oracle,
        deployStorage
      )
      synchronyConstraintChecker = SynchronyConstraintChecker[F](
        conf.server.synchronyConstraintThreshold
      )(Sync[F], blockStore, Log[F])
      lastFinalizedHeightConstraintChecker = LastFinalizedHeightConstraintChecker[F](
        conf.server.heightConstraintThreshold
      )(Sync[F], lastFinalizedStorage, blockStore, Log[F])
      estimator = Estimator[F](conf.casper.maxNumberOfParents, conf.casper.maxParentDepthOpt)(
        Sync[F],
        Log[F],
        Metrics[F],
        span
      )
      runtime <- {
        implicit val s  = rspaceScheduler
        implicit val sp = span
        Runtime.setupRSpace[F](cliConf.storage, cliConf.size) >>= {
          case (space, replay, _) => Runtime.createWithEmptyCost[F]((space, replay), Seq.empty)
        }
      }
      _ <- Runtime.bootstrapRegistry[F](runtime)
      casperRuntimeAndReporter <- {
        implicit val s  = rspaceScheduler
        implicit val sp = span
        implicit val bs = blockStore
        implicit val bd = blockDagStorage
        for {
          sarAndHR            <- Runtime.setupRSpace[F](casperConf.storage, casperConf.size)
          (space, replay, hr) = sarAndHR
          runtime             <- Runtime.createWithEmptyCost[F]((space, replay), Seq.empty)
          reporter = if (conf.server.reporting)
            ReportingCasper.rhoReporter(hr)
          else
            ReportingCasper.noop
        } yield (runtime, reporter)
      }
      (casperRuntime, reportingCasper) = casperRuntimeAndReporter
      runtimeManager <- {
        implicit val sp = span
        RuntimeManager.fromRuntime[F](casperRuntime)
      }
      engineCell   <- EngineCell.init[F]
      envVars      = EnvVars.envVars[F]
      raiseIOError = IOError.raiseIOErrorThroughSync[F]
      casperLaunch = {
        implicit val bs     = blockStore
        implicit val bd     = blockDagStorage
        implicit val lf     = lastFinalizedStorage
        implicit val ec     = engineCell
        implicit val ev     = envVars
        implicit val re     = raiseIOError
        implicit val rb     = requestedBlocks
        implicit val rm     = runtimeManager
        implicit val or     = oracle
        implicit val lc     = lastFinalizedBlockCalculator
        implicit val sp     = span
        implicit val lb     = lab
        implicit val rc     = rpConnections
        implicit val ra     = rpConfAsk
        implicit val eb     = eventPublisher
        implicit val sc     = synchronyConstraintChecker
        implicit val lfhscc = lastFinalizedHeightConstraintChecker
        implicit val cu     = commUtil
        implicit val es     = estimator
        implicit val ds     = deployStorage

        CasperLaunch.of(conf.casper)
      }
      packetHandler <- {
        implicit val ec = engineCell
        implicit val rb = requestedBlocks
        implicit val sp = span
        CasperPacketHandler.fairDispatcher[F](
          conf.roundRobinDispatcher.maxPeerQueueSize,
          conf.roundRobinDispatcher.giveUpAfterSkipped,
          conf.roundRobinDispatcher.dropPeerAfterRetries
        )
      }
      blockApiLock <- Semaphore[F](1)
      apiServers = NodeRuntime.acquireAPIServers[F](runtime, blockApiLock, scheduler)(
        blockStore,
        oracle,
        Concurrent[F],
        Metrics[F],
        span,
        engineCell,
        Log[F],
        Taskable[F],
        synchronyConstraintChecker,
        lastFinalizedHeightConstraintChecker
      )
      casperLoop = for {
        engine <- engineCell.read
        _      <- engine.withCasper(_.fetchDependencies, Applicative[F].unit)
        _ <- Running.maintainRequestedBlocks[F](conf.casper.requestedBlocksTimeout)(
              Monad[F],
              rpConfAsk,
              requestedBlocks,
              TransportLayer[F],
              Log[F],
              Time[F],
              Metrics[F]
            )
        _ <- Time[F].sleep(conf.casper.casperLoopInterval.seconds)
      } yield ()
      engineInit     = engineCell.read >>= (_.init)
      runtimeCleanup = NodeRuntime.cleanup(runtime, casperRuntime, deployStorageCleanup)
      webApi = {
        implicit val ec = engineCell
        implicit val sp = span
        implicit val or = oracle
        implicit val bs = blockStore
        new WebApiImpl[F]
      }
    } yield (
      blockStore,
      blockDagStorage,
      runtimeCleanup,
      packetHandler,
      apiServers,
      casperLoop,
      engineInit,
      casperLaunch,
      reportingCasper,
      webApi
    )

  final case class APIServers(
      repl: ReplGrpcMonix.Repl,
      propose: ProposeServiceV1GrpcMonix.ProposeService,
      deploy: DeployServiceV1GrpcMonix.DeployService
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
      metrics: Metrics[F],
      span: Span[F],
      engineCell: EngineCell[F],
      logF: Log[F],
      taskable: Taskable[F],
      synchronyConstraintChecker: SynchronyConstraintChecker[F],
      lastFinalizedHeightConstraintChecker: LastFinalizedHeightConstraintChecker[F]
  ): APIServers = {
    implicit val s: Scheduler = scheduler
    val repl                  = ReplGrpcService.instance(runtime, s)
    val deploy                = DeployGrpcServiceV1.instance(blockApiLock)
    val propose               = ProposeGrpcServiceV1.instance(blockApiLock)
    APIServers(repl, propose, deploy)
  }
}
