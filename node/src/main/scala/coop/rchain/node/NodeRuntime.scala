package coop.rchain.node

import java.nio.ByteBuffer
import java.nio.file.{Files, Path}

import scala.concurrent.duration._
import cats._
import cats.data.ReaderT
import cats.effect._
import cats.effect.concurrent.{Deferred, Ref, Semaphore}
import cats.implicits._
import cats.mtl._
import cats.tagless.implicits._
import com.typesafe.config.Config
import coop.rchain.blockstorage._
import coop.rchain.blockstorage.casperbuffer.CasperBufferKeyValueStorage
import coop.rchain.blockstorage.dag.{BlockDagFileStorage, BlockDagKeyValueStorage, BlockDagStorage}
import coop.rchain.blockstorage.deploy.LMDBDeployStorage
import coop.rchain.blockstorage.finality.LastFinalizedFileStorage
import coop.rchain.blockstorage.util.io.IOError
import coop.rchain.casper.{ReportingCasper, engine, _}
import coop.rchain.casper.engine.{BlockRetriever, _}
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.protocol.deploy.v1.DeployServiceV1GrpcMonix
import coop.rchain.casper.protocol.propose.v1.ProposeServiceV1GrpcMonix
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
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
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.node.NodeRuntime.{apply => _, _}
import coop.rchain.node.api.WebApi.WebApiImpl
import coop.rchain.node.api.AdminWebApi.AdminWebApiImpl
import coop.rchain.node.api.{
  acquireExternalServer,
  acquireInternalServer,
  AdminWebApi,
  DeployGrpcServiceV1,
  ProposeGrpcServiceV1,
  ReplGrpcService,
  WebApi
}
import coop.rchain.node.configuration.NodeConf
import coop.rchain.node.diagnostics.{
  BatchInfluxDBReporter,
  NewPrometheusReporter,
  Trace,
  UdpInfluxDBReporter
}
import coop.rchain.node.diagnostics.Trace.TraceId
import coop.rchain.node.effects.{EventConsumer, RchainEvents}
import coop.rchain.node.model.repl.ReplGrpcMonix
import coop.rchain.node.web._
import coop.rchain.p2p.effects._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rspace.Context
import coop.rchain.shared._
import coop.rchain.shared.PathOps._
import coop.rchain.store.KeyValueStoreManager
import io.grpc.ManagedChannel
import kamon._
import kamon.system.SystemMetrics
import kamon.zipkin.ZipkinReporter
import monix.eval.Task
import monix.execution.Scheduler
import org.lmdbjava.Env

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class NodeRuntime private[node] (
    nodeConf: NodeConf,
    kamonConf: Config,
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
  private val dataDir                  = nodeConf.storage.dataDir
  private val blockstorePath           = dataDir.resolve("blockstore")
  private val blockdagStoragePath      = dataDir.resolve("dagstorage")
  private val deployStoragePath        = dataDir.resolve("deploystorage")
  private val lastFinalizedStoragePath = dataDir.resolve("last-finalized-block")
  private val rspacePath               = dataDir.resolve("rspace")
  private val casperStoragePath        = rspacePath.resolve("casper")
  private val storageSize              = nodeConf.storage.lmdbMapSizeRspace

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
                nodeConf.protocolServer.host,
                nodeConf.protocolServer.port,
                nodeConf.peersDiscovery.port,
                nodeConf.protocolServer.noUpnp,
                id
              )
              .toReaderT

    // 3. create instances of typeclasses
    metrics       = diagnostics.effects.metrics[Task]
    time          = effects.time[Task]
    commTmpFolder = nodeConf.storage.dataDir.resolve("tmp").resolve("comm")
    _ <- commTmpFolder.toFile
          .exists()
          .fold(
            commTmpFolder.deleteDirectory[Task](),
            Task.unit
          )
          .toReaderT

    clientChannelsCache <- Ref
                            .of[Task, Map[PeerNode, Deferred[Task, BufferedGrpcStreamChannel]]](
                              Map.empty
                            )
                            .toReaderT
    transport <- effects
                  .transportClient(
                    //TODO refactor to accept ProtocolClient, tls and storage configs
                    nodeConf.protocolClient.networkId,
                    nodeConf.tls.certificatePath,
                    nodeConf.tls.keyPath,
                    nodeConf.protocolClient.grpcMaxRecvMessageSize.toInt,
                    nodeConf.protocolClient.grpcStreamChunkSize.toInt,
                    commTmpFolder,
                    clientChannelsCache
                  )(grpcScheduler, log, metrics)
                  .toReaderT
    rpConnections <- effects.rpConnections[Task].toReaderT
    initPeer      = if (nodeConf.standalone) None else Some(nodeConf.protocolClient.bootstrap)
    peerNode      = rpConf(local, initPeer)
    rpConfState   = effects.rpConfState[Task](peerNode)
    peerNodeAsk   = effects.peerNodeAsk[Task](Monad[Task], Sync[Task], rpConfState)
    rpConfAsk     = effects.rpConfAsk[Task](Monad[Task], Sync[Task], rpConfState)
    requestedBlocks <- Ref
                        .of[Task, Map[BlockHash, engine.BlockRetriever.RequestState]](Map.empty)
                        .toReaderT
    commUtil = CommUtil.of[Task](
      Concurrent[Task],
      transport,
      rpConfAsk,
      rpConnections,
      log,
      time
    )
    blockRetriever = BlockRetriever.of[Task](
      Monad[Task],
      requestedBlocks,
      log,
      time,
      rpConfAsk,
      transport,
      commUtil,
      metrics
    )

    kademliaRPC = effects.kademliaRPC(
      nodeConf.protocolServer.networkId,
      nodeConf.protocolClient.networkTimeout,
      nodeConf.protocolServer.allowPrivateAddresses
    )(
      grpcScheduler,
      peerNodeAsk,
      metrics
    )
    kademliaStore = effects.kademliaStore(id)(kademliaRPC, metrics)
    _             <- initPeer.fold(Task.unit)(p => kademliaStore.updateLastSeen(p)).toReaderT
    nodeDiscovery = effects.nodeDiscovery(id)(Monad[Task], kademliaStore, kademliaRPC)

    /**
      * We need to come up with a consistent way with folder creation. Some layers create folder on their own
      * (if not available), others (like blockstore) relay on the structure being created for them (and will fail
      * if it does not exist). For now this small fix should suffice, but we should unify this.
      */
    _             <- mkDirs(dataDir).toReaderT
    _             <- mkDirs(blockstorePath).toReaderT
    _             <- mkDirs(blockdagStoragePath).toReaderT
    blockstoreEnv = Context.env(blockstorePath, nodeConf.storage.lmdbMapSizeBlockstore)
    dagConfig = BlockDagFileStorage.Config(
      latestMessagesLogPath = blockdagStoragePath.resolve("latestMessagesLogPath"),
      latestMessagesCrcPath = blockdagStoragePath.resolve("latestMessagesCrcPath"),
      blockMetadataLogPath = blockdagStoragePath.resolve("blockMetadataLogPath"),
      blockMetadataCrcPath = blockdagStoragePath.resolve("blockMetadataCrcPath"),
      equivocationsTrackerLogPath = blockdagStoragePath.resolve("equivocationsTrackerLogPath"),
      equivocationsTrackerCrcPath = blockdagStoragePath.resolve("equivocationsTrackerCrcPath"),
      invalidBlocksLogPath = blockdagStoragePath.resolve("invalidBlocksLogPath"),
      invalidBlocksCrcPath = blockdagStoragePath.resolve("invalidBlocksCrcPath"),
      blockHashesByDeployLogPath = blockdagStoragePath.resolve("blockHashesByDeployLogPath"),
      blockHashesByDeployCrcPath = blockdagStoragePath.resolve("blockHashesByDeployCrcPath"),
      checkpointsDirPath = blockdagStoragePath.resolve("checkpointsDirPath"),
      blockNumberIndexPath = blockdagStoragePath.resolve("blockNumberIndexPath"),
      mapSize = nodeConf.storage.lmdbMapSizeBlockdagstore,
      latestMessagesLogMaxSizeFactor = 10
    )
    deployStorageConfig = LMDBDeployStorage.Config(
      storagePath = deployStoragePath,
      mapSize = nodeConf.storage.lmdbMapSizeDeploystore
    )
    casperConfig = RuntimeConf(casperStoragePath, storageSize)
    cliConfig    = RuntimeConf(rspacePath, 800L * 1024L * 1024L) // 800MB for cli

    rpConfAskEnv = effects.readerTApplicativeAsk[Task, NodeCallCtx, RPConf](rpConfAsk)
    rpConfStateEnv = effects.readerTMonadState[Task, NodeCallCtx, RPConf](
      rpConfState
    )
    peerNodeAskEnv = effects.readerTApplicativeAsk[Task, NodeCallCtx, PeerNode](
      peerNodeAsk
    )
    metricsEnv        = Metrics.readerTMetrics[Task, NodeCallCtx](metrics)
    transportEnv      = transport.mapK(taskToEnv)
    timeEnv           = time.mapK(taskToEnv)
    logEnv            = log.mapK(taskToEnv)
    eventLogEnv       = eventLog.mapK(taskToEnv)
    nodeDiscoveryEnv  = nodeDiscovery.mapK(taskToEnv)
    rpConnectionsEnv  = Cell.readerT[Task, NodeCallCtx, Connections](rpConnections)
    commUtilEnv       = commUtil.mapK(taskToEnv)
    blockRetrieverEnv = blockRetriever.mapK(taskToEnv)
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
               blockRetrieverEnv,
               nodeConf,
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
      updateForkChoiceLoop,
      engineInit,
      casperLaunch,
      reportingCasper,
      webApi,
      adminWebApi
    ) = result

    // 4. launch casper
    _ <- casperLaunch.launch()

    // 5. run the node program.
    program = nodeProgram(
      apiServers,
      casperLoop,
      updateForkChoiceLoop,
      engineInit,
      runtimeCleanup,
      reportingCasper,
      webApi,
      adminWebApi
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
      blockStore,
      packetHandler,
      eventLogEnv,
      eventBus
    )
    _ <- handleUnrecoverableErrors(program)
  } yield ()

  private def rpConf(local: PeerNode, bootstrapNode: Option[PeerNode]) =
    RPConf(
      local,
      nodeConf.protocolClient.networkId,
      bootstrapNode,
      nodeConf.protocolClient.networkTimeout,
      nodeConf.protocolClient.batchMaxConnections,
      ClearConnectionsConf(nodeConf.peersDiscovery.heartbeatBatchSize)
    )

  // TODO this should use existing algebra
  private def mkDirs(path: Path): Task[Unit] =
    Sync[Task].delay(Files.createDirectories(path))

  private def nodeProgram(
      apiServers: APIServers,
      casperLoop: CasperLoop[TaskEnv],
      updateForkChoiceLoop: CasperLoop[TaskEnv],
      engineInit: EngineInit[TaskEnv],
      runtimeCleanup: Cleanup[TaskEnv],
      reportingCasper: ReportingCasper[TaskEnv],
      webApi: WebApi[TaskEnv],
      adminWebApi: AdminWebApi[TaskEnv]
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
      blockStore: BlockStore[TaskEnv],
      packetHandler: PacketHandler[TaskEnv],
      eventLog: EventLog[TaskEnv],
      consumer: EventConsumer[Task]
  ): TaskEnv[Unit] = {

    val info: TaskEnv[Unit] =
      if (nodeConf.standalone) Log[TaskEnv].info(s"Starting stand-alone node.")
      else
        Log[TaskEnv].info(
          s"Starting node that will bootstrap from ${nodeConf.protocolClient.bootstrap}"
        )

    val dynamicIpCheck: TaskEnv[Unit] =
      if (nodeConf.protocolServer.dynamicIp)
        for {
          local <- peerNodeAsk.ask
          newLocal <- WhoAmI
                       .checkLocalPeerNode[TaskEnv](
                         nodeConf.protocolServer.port,
                         nodeConf.peersDiscovery.port,
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
      servers <- acquireServers(apiServers, reportingCasper, webApi, adminWebApi)(
                  kademliaStore,
                  nodeDiscoveryTask,
                  rpConnections,
                  Concurrent[Task],
                  metrics,
                  rpConfAsk,
                  consumer
                ).toReaderT
      _ <- addShutdownHook(servers, runtimeCleanup, blockStore)
      _ <- servers.externalApiServer.start.toReaderT

      _ <- Log[TaskEnv].info(
            s"External API server started at ${nodeConf.apiServer.host}:${servers.externalApiServer.port}"
          )
      _ <- servers.internalApiServer.start.toReaderT

      _ <- Log[TaskEnv].info(
            s"Internal API server started at ${nodeConf.apiServer.host}:${servers.internalApiServer.port}"
          )
      _ <- servers.kademliaRPCServer.start.toReaderT

      // HTTP server is started immediately on `acquireServers`
      _ <- Log[TaskEnv].info(
            s"HTTP API server started at ${nodeConf.apiServer.host}:${nodeConf.apiServer.portHttp}"
          )

      _ <- Log[TaskEnv].info(
            s"Admin HTTP API server started at ${nodeConf.apiServer.host}:${nodeConf.apiServer.portAdminHttp}"
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
      _ <- if (nodeConf.standalone) ().pure[TaskEnv]
          else Log[TaskEnv].info(s"Waiting for first connection.") >> waitForFirstConnection
      _ <- Concurrent[TaskEnv].start(engineInit)
      _ <- Task
            .defer(casperLoop.forever.run(NodeCallCtx.init))
            .executeOn(loopScheduler)
            .start
            .toReaderT
      _ <- Task
            .defer(updateForkChoiceLoop.forever.run(NodeCallCtx.init))
            .executeOn(loopScheduler)
            .toReaderT
    } yield ()
  }

  def addShutdownHook(
      servers: Servers,
      runtimeCleanup: Cleanup[TaskEnv],
      blockStore: BlockStore[TaskEnv]
  )(implicit log: Log[TaskEnv]): TaskEnv[Unit] =
    Task
      .delay(sys.addShutdownHook(clearResources(servers, runtimeCleanup, blockStore)))
      .as(())
      .toReaderT

  def clearResources(
      servers: Servers,
      runtimeCleanup: Cleanup[TaskEnv],
      blockStore: BlockStore[TaskEnv]
  )(implicit log: Log[TaskEnv]): Unit =
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
      httpServer: Fiber[Task, Unit],
      adminHttpServer: Fiber[Task, Unit]
  )

  def acquireServers(
      apiServers: APIServers,
      reportingCasper: ReportingCasper[TaskEnv],
      webApi: WebApi[TaskEnv],
      adminWebApi: AdminWebApi[TaskEnv]
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
                              nodeConf.protocolServer.networkId,
                              nodeConf.peersDiscovery.port,
                              KademliaHandleRPC.handlePing[Task],
                              KademliaHandleRPC.handleLookup[Task]
                            )(grpcScheduler)

      transportServer <- Task
                          .delay(
                            GrpcTransportServer.acquireServer(
                              //conf.protocolServer
                              //conf.tls
                              //conf.storage
                              nodeConf.protocolServer.networkId,
                              nodeConf.protocolServer.port,
                              nodeConf.tls.certificatePath,
                              nodeConf.tls.keyPath,
                              nodeConf.protocolServer.grpcMaxRecvMessageSize.toInt,
                              nodeConf.protocolServer.grpcMaxRecvStreamMessageSize,
                              nodeConf.storage.dataDir.resolve("tmp").resolve("comm"),
                              nodeConf.protocolServer.maxMessageConsumers
                            )(grpcScheduler, rPConfAsk, log, metrics)
                          )

      externalApiServer <- acquireExternalServer[Task](
                            //conf.apiServer
                            nodeConf.apiServer.host,
                            nodeConf.apiServer.portGrpcExternal,
                            grpcScheduler,
                            apiServers.deploy,
                            nodeConf.apiServer.grpcMaxRecvMessageSize.toInt,
                            nodeConf.apiServer.keepAliveTime,
                            nodeConf.apiServer.keepAliveTimeout,
                            nodeConf.apiServer.permitKeepAliveTime,
                            nodeConf.apiServer.maxConnectionIdle,
                            nodeConf.apiServer.maxConnectionAge,
                            nodeConf.apiServer.maxConnectionAgeGrace
                          )
      internalApiServer <- acquireInternalServer(
                            nodeConf.apiServer.host,
                            nodeConf.apiServer.portGrpcInternal,
                            grpcScheduler,
                            apiServers.repl,
                            apiServers.deploy,
                            apiServers.propose,
                            nodeConf.apiServer.grpcMaxRecvMessageSize.toInt,
                            nodeConf.apiServer.keepAliveTime,
                            nodeConf.apiServer.keepAliveTimeout,
                            nodeConf.apiServer.permitKeepAliveTime,
                            nodeConf.apiServer.maxConnectionIdle,
                            nodeConf.apiServer.maxConnectionAge,
                            nodeConf.apiServer.maxConnectionAgeGrace
                          )

      prometheusReporter = new NewPrometheusReporter()
      httpServerFiber = aquireHttpServer(
        nodeConf.apiServer.enableReporting,
        nodeConf.apiServer.host,
        nodeConf.apiServer.portHttp,
        prometheusReporter,
        reportingCasper,
        webApi,
        nodeConf.apiServer.maxConnectionIdle
      )(nodeDiscovery, connectionsCell, concurrent, rPConfAsk, consumer, s, log)
      httpFiber <- httpServerFiber.start
      adminHttpServerFiber = aquireAdminHttpServer(
        nodeConf.apiServer.host,
        nodeConf.apiServer.portAdminHttp,
        adminWebApi,
        nodeConf.apiServer.maxConnectionIdle
      )(concurrent, consumer, s)
      adminHttpFiber <- adminHttpServerFiber.start
      _ <- Task.delay {
            Kamon.reconfigure(kamonConf.withFallback(Kamon.config()))
            if (nodeConf.metrics.influxdb) Kamon.addReporter(new BatchInfluxDBReporter())
            if (nodeConf.metrics.influxdbUdp) Kamon.addReporter(new UdpInfluxDBReporter())
            if (nodeConf.metrics.prometheus) Kamon.addReporter(prometheusReporter)
            if (nodeConf.metrics.zipkin) Kamon.addReporter(new ZipkinReporter())
            if (nodeConf.metrics.sigar) SystemMetrics.startCollecting()
          }
    } yield Servers(
      kademliaRPCServer,
      transportServer,
      externalApiServer,
      internalApiServer,
      httpFiber,
      adminHttpFiber
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
      nodeConf: NodeConf,
      kamonConf: Config
  )(implicit scheduler: Scheduler, log: Log[Task], eventLog: EventLog[Task]): Task[NodeRuntime] =
    for {
      id      <- NodeEnvironment.create(nodeConf)
      runtime <- Task.delay(new NodeRuntime(nodeConf, kamonConf, id, scheduler))
    } yield runtime

  trait Cleanup[F[_]] {
    def close(): F[Unit]
  }

  def cleanup[F[_]: Sync: Log](
      runtime: Runtime[F],
      casperRuntime: Runtime[F],
      deployStorageCleanup: F[Unit],
      casperStoreManager: KeyValueStoreManager[F]
  ): Cleanup[F] =
    new Cleanup[F] {
      override def close(): F[Unit] =
        for {
          _ <- Log[F].info("Shutting down interpreter runtime ...")
          _ <- runtime.close()
          _ <- Log[F].info("Shutting down Casper runtime ...")
          _ <- casperRuntime.close()
          _ <- Log[F].info("Shutting down Casper store manager ...")
          _ <- casperStoreManager.shutdown
          _ <- Log[F].info("Shutting down deploy storage ...")
          _ <- deployStorageCleanup
        } yield ()
    }

  def setupNodeProgramF[F[_]: Metrics: TransportLayer: Sync: Concurrent: Time: Log: EventLog: ContextShift: Parallel: Taskable: LocalEnvironment](
      rpConnections: ConnectionsCell[F],
      rpConfAsk: ApplicativeAsk[F, RPConf],
      rpConfState: MonadState[F, RPConf],
      commUtil: CommUtil[F],
      blockRetriever: BlockRetriever[F],
      conf: NodeConf,
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
        BlockDagStorage[F],
        Cleanup[F],
        PacketHandler[F],
        APIServers,
        CasperLoop[F],
        CasperLoop[F],
        EngineInit[F],
        CasperLaunch[F],
        ReportingCasper[F],
        WebApi[F],
        AdminWebApi[F]
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
      span = if (conf.metrics.zipkin)
        diagnostics.effects
          .span(conf.protocolServer.networkId, conf.protocolServer.host.getOrElse("-"))
      else Span.noop[F]
      // Key-value store manager / manages LMDB databases
      casperStoreManager <- RNodeKeyValueStoreManager(conf.storage.dataDir)
      blockDagStorage <- {
        implicit val kvm = casperStoreManager
        for {
          // Check if migration from DAG file storage to LMDB should be executed
          blockMetadataDb   <- casperStoreManager.store("block-metadata")
          dagStorageIsEmpty = blockMetadataDb.iterate(_.isEmpty)
          oldStorageExists  = Sync[F].delay(Files.exists(dagConfig.blockMetadataLogPath))
          shouldMigrate     <- dagStorageIsEmpty &&^ oldStorageExists
          // TODO: remove `dagConfig`, it's not used anymore (after migration)
          _ <- BlockDagKeyValueStorage.importFromFileStorage(dagConfig).whenA(shouldMigrate)
          // Create DAG store
          dagStorage <- BlockDagKeyValueStorage.create[F]
        } yield dagStorage
      }
      casperBufferStorage <- {
        implicit val kvm = casperStoreManager
        CasperBufferKeyValueStorage.create[F]
      }
      lastFinalizedStorage                  <- LastFinalizedFileStorage.make[F](lastFinalizedPath)
      deployStorageAllocation               <- LMDBDeployStorage.make[F](deployStorageConfig).allocated
      (deployStorage, deployStorageCleanup) = deployStorageAllocation
      oracle = {
        implicit val sp = span
        SafetyOracle.cliqueOracle[F]
      }
      lastFinalizedBlockCalculator = LastFinalizedBlockCalculator[F](
        conf.casper.faultToleranceThreshold
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
        conf.casper.synchronyConstraintThreshold
      )(Sync[F], blockStore, Log[F])
      lastFinalizedHeightConstraintChecker = LastFinalizedHeightConstraintChecker[F](
        conf.casper.heightConstraintThreshold
      )(Sync[F], lastFinalizedStorage, blockStore, Log[F])
      estimator = Estimator[F](conf.casper.maxNumberOfParents, conf.casper.maxParentDepth)(
        Sync[F],
        Log[F],
        Metrics[F],
        span
      )
      evalRuntime <- {
        implicit val s  = rspaceScheduler
        implicit val sp = span
        Runtime.setupRSpace[F](cliConf.storage, cliConf.size) >>= {
          case (space, replay, _) => Runtime.createWithEmptyCost[F]((space, replay), Seq.empty)
        }
      }
      _ <- Runtime.bootstrapRegistry[F](evalRuntime)
      casperRuntimeAndReporter <- {
        implicit val s  = rspaceScheduler
        implicit val sp = span
        implicit val bs = blockStore
        implicit val bd = blockDagStorage
        for {
          sarAndHR            <- Runtime.setupRSpace[F](casperConf.storage, casperConf.size)
          (space, replay, hr) = sarAndHR
          runtime             <- Runtime.createWithEmptyCost[F]((space, replay), Seq.empty)
          reporter <- if (conf.apiServer.enableReporting) {
                       import coop.rchain.rholang.interpreter.storage._
                       implicit val kvm = casperStoreManager
                       for {
                         reportingCache <- ReportMemStore
                                            .store[
                                              F,
                                              Par,
                                              BindPattern,
                                              ListParWithRandom,
                                              TaggedContinuation
                                            ]
                       } yield ReportingCasper.rhoReporter(hr, reportingCache)
                     } else
                       ReportingCasper.noop.pure[F]
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
        implicit val br     = blockRetriever
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
        implicit val cbs    = casperBufferStorage

        CasperLaunch.of[F](conf.casper)
      }
      packetHandler = {
        implicit val ec = engineCell
        CasperPacketHandler[F]
      }
      // Bypass fair dispatcher
      /*packetHandler <- {
        implicit val ec = engineCell
        implicit val rb = requestedBlocks
        implicit val sp = span
        CasperPacketHandler.fairDispatcher[F](
          conf.roundRobinDispatcher.maxPeerQueueSize,
          conf.roundRobinDispatcher.giveUpAfterSkipped,
          conf.roundRobinDispatcher.dropPeerAfterRetries
        )
      }*/
      blockApiLock <- Semaphore[F](1)
      apiServers = NodeRuntime
        .acquireAPIServers[F](
          evalRuntime,
          blockApiLock,
          scheduler,
          conf.apiServer.maxBlocksLimit,
          conf.devMode
        )(
          blockStore,
          oracle,
          Concurrent[F],
          Metrics[F],
          span,
          engineCell,
          Log[F],
          Taskable[F],
          synchronyConstraintChecker,
          lastFinalizedHeightConstraintChecker,
          reportingCasper
        )
      casperLoop = {
        implicit val br = blockRetriever
        for {
          engine <- engineCell.read
          // Fetch dependencies from CasperBuffer
          _ <- engine.withCasper(_.fetchDependencies, Applicative[F].unit)
          // Maintain RequestedBlocks for Casper
          _ <- BlockRetriever[F].requestAll(conf.casper.requestedBlocksTimeout)
          _ <- Time[F].sleep(conf.casper.casperLoopInterval)
        } yield ()
      }
      // Broadcast fork choice tips request if current fork choice is more then `forkChoiceStaleThreshold` minutes old.
      // For why - look at updateForkChoiceTipsIfStuck method description.
      updateForkChoiceLoop = {
        implicit val cu = commUtil
        implicit val ec = engineCell
        implicit val bs = blockStore
        for {
          _ <- Time[F].sleep(conf.casper.forkChoiceCheckIfStaleInterval)
          _ <- Running.updateForkChoiceTipsIfStuck(conf.casper.forkChoiceStaleThreshold)
        } yield ()
      }
      engineInit = engineCell.read >>= (_.init)
      runtimeCleanup = NodeRuntime.cleanup(
        evalRuntime,
        casperRuntime,
        deployStorageCleanup,
        casperStoreManager
      )
      webApi = {
        implicit val ec = engineCell
        implicit val sp = span
        implicit val or = oracle
        implicit val bs = blockStore
        new WebApiImpl[F](conf.apiServer.maxBlocksLimit, conf.devMode)
      }
      adminWebApi = {
        implicit val ec     = engineCell
        implicit val sp     = span
        implicit val sc     = synchronyConstraintChecker
        implicit val lfhscc = lastFinalizedHeightConstraintChecker
        new AdminWebApiImpl[F](blockApiLock)
      }
    } yield (
      blockStore,
      blockDagStorage,
      runtimeCleanup,
      packetHandler,
      apiServers,
      casperLoop,
      updateForkChoiceLoop,
      engineInit,
      casperLaunch,
      reportingCasper,
      webApi,
      adminWebApi
    )

  final case class APIServers(
      repl: ReplGrpcMonix.Repl,
      propose: ProposeServiceV1GrpcMonix.ProposeService,
      deploy: DeployServiceV1GrpcMonix.DeployService
  )

  def acquireAPIServers[F[_]](
      runtime: Runtime[F],
      blockApiLock: Semaphore[F],
      scheduler: Scheduler,
      apiMaxBlocksLimit: Int,
      devMode: Boolean
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
      lastFinalizedHeightConstraintChecker: LastFinalizedHeightConstraintChecker[F],
      reportingCasper: ReportingCasper[F]
  ): APIServers = {
    implicit val s: Scheduler = scheduler
    val repl                  = ReplGrpcService.instance(runtime, s)
    val deploy =
      DeployGrpcServiceV1.instance(blockApiLock, apiMaxBlocksLimit, reportingCasper, devMode)
    val propose = ProposeGrpcServiceV1.instance(blockApiLock)
    APIServers(repl, propose, deploy)
  }
}
