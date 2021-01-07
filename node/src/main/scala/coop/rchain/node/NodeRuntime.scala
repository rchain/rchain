package coop.rchain.node

import java.nio.file.{Files, Path}

import cats.data.ReaderT
import cats.effect._
import cats.effect.concurrent.{Ref, Semaphore}
import cats.effect.syntax.all._
import cats.mtl._
import cats.syntax.all._
import cats.{~>, Parallel}
import com.typesafe.config.Config
import coop.rchain.blockstorage._
import coop.rchain.blockstorage.casperbuffer.CasperBufferKeyValueStorage
import coop.rchain.blockstorage.dag.{BlockDagFileStorage, BlockDagKeyValueStorage}
import coop.rchain.blockstorage.deploy.LMDBDeployStorage
import coop.rchain.blockstorage.finality.{LastFinalizedFileStorage, LastFinalizedKeyValueStorage}
import coop.rchain.blockstorage.util.io.IOError
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.engine.{BlockRetriever, _}
import coop.rchain.casper.protocol.deploy.v1.DeployServiceV1GrpcMonix
import coop.rchain.casper.protocol.propose.v1.ProposeServiceV1GrpcMonix
import coop.rchain.casper.state.instances.BlockStateManagerImpl
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.casper.util.comm._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.{ReportingCasper, engine, _}
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
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.monix.Monixable
import coop.rchain.node.NodeRuntime._
import coop.rchain.node.api.AdminWebApi.AdminWebApiImpl
import coop.rchain.node.api.WebApi.WebApiImpl
import coop.rchain.node.api._
import coop.rchain.node.configuration.NodeConf
import coop.rchain.node.diagnostics.Trace.TraceId
import coop.rchain.node.diagnostics.{
  BatchInfluxDBReporter,
  NewPrometheusReporter,
  Trace,
  UdpInfluxDBReporter
}
import coop.rchain.node.effects.{EventConsumer, RchainEvents}
import coop.rchain.node.model.repl.ReplGrpcMonix
import coop.rchain.node.state.instances.RNodeStateManagerImpl
import coop.rchain.node.web._
import coop.rchain.p2p.effects._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rspace.Context
import coop.rchain.rspace.state.instances.RSpaceStateManagerImpl
import coop.rchain.shared._
import coop.rchain.shared.syntax._
import coop.rchain.store.KeyValueStoreManager
import kamon._
import kamon.instrumentation.system
import kamon.zipkin.ZipkinReporter
import monix.execution.Scheduler

import scala.concurrent.duration._

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class NodeRuntime[F[_]: Monixable: ConcurrentEffect: Parallel: Timer: ContextShift: LocalEnvironment: Log: EventLog] private[node] (
    nodeConf: NodeConf,
    kamonConf: Config,
    id: NodeIdentifier,
    scheduler: Scheduler
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

  /**
    * Main node entry. It will:
    * 1. set up configurations
    * 2. create instances of typeclasses
    * 3. run the node program.
    */
  // TODO: Resolve scheduler chaos in Runtime, RuntimeManager and CasperPacketHandler
  def main: F[Unit] = {
    for {
      // 1. fetch local peer node
      local <- WhoAmI
                .fetchLocalPeerNode[F](
                  nodeConf.protocolServer.host,
                  nodeConf.protocolServer.port,
                  nodeConf.peersDiscovery.port,
                  nodeConf.protocolServer.noUpnp,
                  id
                )

      // 3. create instances of typeclasses
      metrics = diagnostics.effects.metrics[F]
      time    = effects.time[F]

      transport <- {
        implicit val s = scheduler
        implicit val m = metrics
        effects
          .transportClient[F](
            //TODO refactor to accept ProtocolClient, tls and storage configs
            nodeConf.protocolClient.networkId,
            nodeConf.tls.certificatePath,
            nodeConf.tls.keyPath,
            nodeConf.protocolClient.grpcMaxRecvMessageSize.toInt,
            nodeConf.protocolClient.grpcStreamChunkSize.toInt,
            grpcScheduler
          )
      }

      rpConnections   <- effects.rpConnections[F]
      initPeer        = if (nodeConf.standalone) None else Some(nodeConf.protocolClient.bootstrap)
      peerNode        = rpConf(local, initPeer)
      rpConfState     = effects.rpConfState[F](peerNode)
      peerNodeAsk     = effects.peerNodeAsk[F](Sync[F], rpConfState)
      rpConfAsk       = effects.rpConfAsk[F](Sync[F], rpConfState)
      requestedBlocks <- Ref.of[F, Map[BlockHash, engine.BlockRetriever.RequestState]](Map.empty)

      commUtil = {
        implicit val tr = transport
        implicit val cn = rpConfAsk
        implicit val cc = rpConnections
        implicit val ti = time
        CommUtil.of[F]
      }

      blockRetriever = {
        implicit val me = metrics
        implicit val tr = transport
        implicit val ti = time
        implicit val cn = rpConfAsk
        implicit val cu = commUtil
        implicit val rb = requestedBlocks
        BlockRetriever.of[F]
      }

      kademliaRPC = {
        implicit val s = grpcScheduler
        implicit val m = metrics
        implicit val p = peerNodeAsk
        effects.kademliaRPC(
          nodeConf.protocolServer.networkId,
          nodeConf.protocolClient.networkTimeout,
          nodeConf.protocolServer.allowPrivateAddresses
        )
      }

      kademliaStore = {
        implicit val k = kademliaRPC
        implicit val m = metrics
        effects.kademliaStore(id)
      }

      _             <- initPeer.fold(().pure[F])(p => kademliaStore.updateLastSeen(p))
      nodeDiscovery = effects.nodeDiscovery(id)(Sync[F], kademliaStore, kademliaRPC)

      /**
        * We need to come up with a consistent way with folder creation. Some layers create folder on their own
        * (if not available), others (like blockstore) relay on the structure being created for them (and will fail
        * if it does not exist). For now this small fix should suffice, but we should unify this.
        */
      _ <- mkDirs(dataDir)
      _ <- mkDirs(blockdagStoragePath)

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
        mapSize = nodeConf.storage.lmdbMapSizeBlockdagstore
      )
      deployStorageConfig = LMDBDeployStorage.Config(
        storagePath = deployStoragePath,
        mapSize = nodeConf.storage.lmdbMapSizeDeploystore
      )
      casperConfig = RuntimeConf(casperStoragePath, storageSize)
      cliConfig    = RuntimeConf(rspacePath, 800L * 1024L * 1024L) // 800MB for cli

      eventBus <- RchainEvents[F]

      result <- {
        implicit val tr = transport
        implicit val ti = time
        implicit val me = metrics
        NodeRuntime.setupNodeProgram[F](
          rpConnections,
          rpConfAsk,
          commUtil,
          blockRetriever,
          nodeConf,
          dagConfig,
          casperConfig,
          cliConfig,
          blockstorePath,
          lastFinalizedStoragePath,
          rspaceScheduler,
          scheduler,
          eventBus,
          deployStorageConfig
        )
      }
      (
        blockStore,
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
      program = {
        implicit val tr = transport
        implicit val cn = rpConfAsk
        implicit val cc = rpConnections
        implicit val cs = rpConfState
        implicit val pn = peerNodeAsk
        implicit val ks = kademliaStore
        implicit val nd = nodeDiscovery
        implicit val bs = blockStore
        implicit val ph = packetHandler
        implicit val eb = eventBus
        implicit val mt = metrics
        implicit val ti = time
        nodeProgram(
          apiServers,
          casperLoop,
          updateForkChoiceLoop,
          engineInit,
          runtimeCleanup,
          reportingCasper,
          webApi,
          adminWebApi
        )
      }
      _ <- handleUnrecoverableErrors(program)
    } yield ()
  }

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
  private def mkDirs(path: Path): F[Unit] =
    Sync[F].delay(Files.createDirectories(path))

  private def nodeProgram(
      apiServers: APIServers,
      casperLoop: CasperLoop[F],
      updateForkChoiceLoop: CasperLoop[F],
      engineInit: EngineInit[F],
      runtimeCleanup: Cleanup[F],
      reportingCasper: ReportingCasper[F],
      webApi: WebApi[F],
      adminWebApi: AdminWebApi[F]
  )(
      implicit
      time: Time[F],
      rpConfState: RPConfState[F],
      rpConfAsk: RPConfAsk[F],
      peerNodeAsk: PeerNodeAsk[F],
      metrics: Metrics[F],
      transport: TransportLayer[F],
      kademliaStore: KademliaStore[F],
      nodeDiscovery: NodeDiscovery[F],
      rpConnections: ConnectionsCell[F],
      blockStore: BlockStore[F],
      packetHandler: PacketHandler[F],
      consumer: EventConsumer[F]
  ): F[Unit] = {

    val info: F[Unit] =
      if (nodeConf.standalone) Log[F].info(s"Starting stand-alone node.")
      else
        Log[F].info(
          s"Starting node that will bootstrap from ${nodeConf.protocolClient.bootstrap}"
        )

    val dynamicIpCheck: F[Unit] =
      if (nodeConf.protocolServer.dynamicIp)
        for {
          local <- peerNodeAsk.ask
          newLocal <- WhoAmI
                       .checkLocalPeerNode[F](
                         nodeConf.protocolServer.port,
                         nodeConf.peersDiscovery.port,
                         local
                       )
          _ <- newLocal.fold(().pure[F]) { pn =>
                Connect
                  .resetConnections[F]
                  .flatMap(kp(rpConfState.modify(_.copy(local = pn))))
              }
        } yield ()
      else ().pure[F]

    val nodeDiscoveryLoop: F[Unit] =
      for {
        _ <- NodeDiscovery[F].discover
        _ <- Connect.findAndConnect[F](Connect.connect[F])
        _ <- time.sleep(20.seconds)
      } yield ()

    val clearConnectionsLoop: F[Unit] =
      for {
        _ <- dynamicIpCheck
        _ <- Connect.clearConnections[F]
        _ <- time.sleep(10.minutes)
      } yield ()

    def waitForFirstConnection: F[Unit] =
      for {
        _ <- time.sleep(10.second)
        _ <- ConnectionsCell[F].read
              .map(_.isEmpty)
              .ifM(waitForFirstConnection, ().pure[F])
      } yield ()

    for {
      _       <- info
      local   <- peerNodeAsk.ask
      host    = local.endpoint.host
      servers <- acquireServers(apiServers, reportingCasper, webApi, adminWebApi)
      _       <- addShutdownHook(servers, runtimeCleanup, blockStore)
      _       <- servers.externalApiServer.start

      _ <- Log[F].info(
            s"External API server started at ${nodeConf.apiServer.host}:${servers.externalApiServer.port}"
          )
      _ <- servers.internalApiServer.start

      _ <- Log[F].info(
            s"Internal API server started at ${nodeConf.apiServer.host}:${servers.internalApiServer.port}"
          )
      _ <- servers.kademliaRPCServer.start

      // HTTP server is started immediately on `acquireServers`
      _ <- Log[F].info(
            s"HTTP API server started at ${nodeConf.apiServer.host}:${nodeConf.apiServer.portHttp}"
          )

      _ <- Log[F].info(
            s"Admin HTTP API server started at ${nodeConf.apiServer.host}:${nodeConf.apiServer.portAdminHttp}"
          )

      _ <- Log[F].info(
            s"Kademlia RPC server started at $host:${servers.kademliaRPCServer.port}"
          )

      _ <- servers.transportServer
            .start(
              HandleMessages.handle[F](_),
              blob => packetHandler.handlePacket(blob.sender, blob.packet)
            )

      address = local.toAddress
      _       <- Log[F].info(s"Listening for traffic on $address.")
      _       <- EventLog[F].publish(Event.NodeStarted(address))
      _ <- Sync[F]
            .defer(nodeDiscoveryLoop.forever)
            .toTask
            .executeOn(loopScheduler)
            .fromTask
            .start
      _ <- Sync[F]
            .defer(clearConnectionsLoop.forever)
            .toTask
            .executeOn(loopScheduler)
            .fromTask
            .start
      _ <- if (nodeConf.standalone) ().pure[F]
          else Log[F].info(s"Waiting for first connection.") >> waitForFirstConnection
      _ <- Concurrent[F].start(engineInit)
      _ <- Sync[F]
            .defer(casperLoop.forever)
            .toTask
            .executeOn(loopScheduler)
            .fromTask
            .start
      _ <- Sync[F]
            .defer(updateForkChoiceLoop.forever)
            .toTask
            .executeOn(loopScheduler)
            .fromTask
    } yield ()
  }

  def addShutdownHook(
      servers: Servers,
      runtimeCleanup: Cleanup[F],
      blockStore: BlockStore[F]
  ): F[Unit] =
    Sync[F].delay(sys.addShutdownHook(clearResources(servers, runtimeCleanup, blockStore))).void

  def clearResources(
      servers: Servers,
      runtimeCleanup: Cleanup[F],
      blockStore: BlockStore[F]
  ): Unit = {
    val shutdown = for {
      _ <- Log[F].info("Shutting down API servers...")
      _ <- servers.externalApiServer.stop
      _ <- servers.internalApiServer.stop
      _ <- Log[F].info("Shutting down Kademlia RPC server...")
      _ <- servers.kademliaRPCServer.stop
      _ <- Log[F].info("Shutting down transport layer...")
      _ <- servers.transportServer.stop()
      _ <- Log[F].info("Shutting down HTTP server....")
      _ <- Sync[F].delay(Kamon.stopModules())
      _ <- servers.httpServer.cancel.attempt
      _ <- runtimeCleanup.close()
      _ <- Log[F].info("Bringing BlockStore down ...")
      _ <- blockStore.close()
      _ <- Log[F].info("Goodbye.")
    } yield ()

    shutdown.toTask.unsafeRunSync(scheduler)
  }

  private def exit0: F[Unit] = Sync[F].delay(System.exit(0))

  /**
    * Handles unrecoverable errors in program. Those are errors that should not happen in properly
    * configured enviornment and they mean immediate termination of the program
    */
  private def handleUnrecoverableErrors(prog: F[Unit]): F[Unit] =
    prog.handleErrorWith { ex =>
      Log[F].error("Caught unhandable error. Exiting. Stacktrace below.", ex)
    } >> exit0.void

  case class Servers(
      kademliaRPCServer: Server[F],
      transportServer: TransportServer[F],
      externalApiServer: Server[F],
      internalApiServer: Server[F],
      httpServer: Fiber[F, Unit],
      adminHttpServer: Fiber[F, Unit]
  )

  def acquireServers(
      apiServers: APIServers,
      reportingCasper: ReportingCasper[F],
      webApi: WebApi[F],
      adminWebApi: AdminWebApi[F]
  )(
      implicit
      kademliaStore: KademliaStore[F],
      nodeDiscovery: NodeDiscovery[F],
      connectionsCell: ConnectionsCell[F],
      metrics: Metrics[F],
      rPConfAsk: RPConfAsk[F],
      consumer: EventConsumer[F]
  ): F[Servers] = {
    implicit val s: Scheduler = scheduler
    for {
      kademliaRPCServer <- discovery
                            .acquireKademliaRPCServer(
                              nodeConf.protocolServer.networkId,
                              nodeConf.peersDiscovery.port,
                              KademliaHandleRPC.handlePing[F],
                              KademliaHandleRPC.handleLookup[F]
                            )

      transportServer <- Sync[F]
                          .delay(
                            GrpcTransportServer.acquireServer(
                              nodeConf.protocolServer.networkId,
                              nodeConf.protocolServer.port,
                              nodeConf.tls.certificatePath,
                              nodeConf.tls.keyPath,
                              nodeConf.protocolServer.grpcMaxRecvMessageSize.toInt,
                              nodeConf.protocolServer.grpcMaxRecvStreamMessageSize,
                              nodeConf.protocolServer.maxMessageConsumers
                            )
                          )

      externalApiServer <- acquireExternalServer[F](
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
      internalApiServer <- acquireInternalServer[F](
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
      httpServerFiber = aquireHttpServer[F](
        nodeConf.apiServer.enableReporting,
        nodeConf.apiServer.host,
        nodeConf.apiServer.portHttp,
        prometheusReporter,
        reportingCasper,
        webApi,
        nodeConf.apiServer.maxConnectionIdle
      )
      httpFiber <- httpServerFiber.start
      adminHttpServerFiber = aquireAdminHttpServer[F](
        nodeConf.apiServer.host,
        nodeConf.apiServer.portAdminHttp,
        adminWebApi,
        nodeConf.apiServer.maxConnectionIdle
      )
      adminHttpFiber <- adminHttpServerFiber.start
      _ <- Sync[F].delay {
            Kamon.reconfigure(kamonConf.withFallback(Kamon.config()))
            if (nodeConf.metrics.influxdb)
              Kamon.registerModule("BatchInfluxDBReporter", new BatchInfluxDBReporter())
            if (nodeConf.metrics.influxdbUdp)
              Kamon.registerModule("UdpInfluxDBReporter", new UdpInfluxDBReporter())
            if (nodeConf.metrics.prometheus)
              Kamon.registerModule("prometheusReporter", prometheusReporter)
            if (nodeConf.metrics.zipkin)
              Kamon.registerModule("prometheusReporter", new ZipkinReporter())
            Kamon.init()
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
  type LocalEnvironment[F[_]] = ApplicativeLocal[F, NodeCallCtx]

  type CasperLoop[F[_]] = F[Unit]
  type EngineInit[F[_]] = F[Unit]

  final case class RuntimeConf(
      storage: Path,
      size: Long
  )

  def start[F[_]: Monixable: ConcurrentEffect: Parallel: ContextShift: Timer: Log: EventLog](
      nodeConf: NodeConf,
      kamonConf: Config
  )(implicit scheduler: Scheduler): F[Unit] = {

    /**
      * Current implementation of Span uses ReaderT layer to hold the local state for tracing.
      *
      * To be able to instantiate NodeRuntime dependencies we need ReaderT implementation for each of them.
      * If it's possible to construct FunctorK implementation like we have for Log then this can be used as a
      * more general implementation.
      */
    type TaskEnv[A] = ReaderT[F, NodeCallCtx, A]

    // Conversions from/to ReaderT and F
    val taskToEnv: F ~> TaskEnv = λ[F ~> TaskEnv](ReaderT.liftF(_))
    val envToTask: TaskEnv ~> F = λ[TaskEnv ~> F](x => x.run(NodeCallCtx.init))

    implicit val localEnvironment = cats.mtl.instances.all.localReader[F, NodeCallCtx]

    /**
      * Implementation for ConcurrentEffect for ReaderT cannot be constructed automatically so it's
      * wired up here from existing Concurrent[ReaderT[F, S, ?]] and ConcurrentEffect[F] implementations.
      *
      * `runCancelable`` and `runAsync` are newly provided.
      */
    implicit val ce = new ConcurrentEffect[TaskEnv] {
      val c = Concurrent[TaskEnv]
      val t = ConcurrentEffect[F]

      // ConcurrentEffect
      override def runCancelable[A](fa: TaskEnv[A])(
          cb: Either[Throwable, A] => IO[Unit]
      ): SyncIO[CancelToken[TaskEnv]] =
        t.runCancelable(envToTask(fa))(cb).map(taskToEnv(_))
      override def runAsync[A](
          fa: TaskEnv[A]
      )(cb: Either[Throwable, A] => IO[Unit]): SyncIO[Unit] =
        t.runAsync(envToTask(fa))(cb)
      // Async
      override def async[A](k: (Either[Throwable, A] => Unit) => Unit): TaskEnv[A] = c.async(k)
      override def asyncF[A](k: (Either[Throwable, A] => Unit) => TaskEnv[Unit]): TaskEnv[A] =
        c.asyncF(k)
      // Concurrent
      override def start[A](fa: TaskEnv[A]): TaskEnv[Fiber[TaskEnv, A]] = c.start(fa)
      override def racePair[A, B](
          fa: TaskEnv[A],
          fb: TaskEnv[B]
      ): TaskEnv[Either[(A, Fiber[TaskEnv, B]), (Fiber[TaskEnv, A], B)]] = c.racePair(fa, fb)
      override def suspend[A](thunk: => TaskEnv[A]): TaskEnv[A]          = c.suspend(thunk)
      override def bracketCase[A, B](acquire: TaskEnv[A])(use: A => TaskEnv[B])(
          release: (A, ExitCase[Throwable]) => TaskEnv[Unit]
      ): TaskEnv[B]                                        = c.bracketCase(acquire)(use)(release)
      override def raiseError[A](e: Throwable): TaskEnv[A] = c.raiseError(e)
      override def handleErrorWith[A](fa: TaskEnv[A])(f: Throwable => TaskEnv[A]): TaskEnv[A] =
        c.handleErrorWith(fa)(f)
      override def flatMap[A, B](fa: TaskEnv[A])(f: A => TaskEnv[B]): TaskEnv[B] =
        c.flatMap(fa)(f)
      override def tailRecM[A, B](a: A)(f: A => TaskEnv[Either[A, B]]): TaskEnv[B] =
        c.tailRecM(a)(f)
      override def pure[A](x: A): TaskEnv[A] = c.pure(x)
    }

    /**
      * ReaderT instances for NodeRuntime dependencies. Implementations for Log and EventLog are created "manually"
      * although they can be generated with cats.tagless @autoFunctorK macros but support is missing for IntelliJ.
      * https://github.com/typelevel/cats-tagless/issues/60 (Cheers, Marcin!!)
      */
    implicit val lg: Log[TaskEnv]       = Log[F].mapK(taskToEnv)
    implicit val el: EventLog[TaskEnv]  = EventLog[F].mapK(taskToEnv)
    implicit val tm: Timer[TaskEnv]     = Timer[F].mapK(taskToEnv)
    implicit val mn: Monixable[TaskEnv] = Monixable[F].mapK(taskToEnv, NodeCallCtx.init)

    for {
      id <- NodeEnvironment.create[F](nodeConf)

      // Create NodeRuntime instance
      runtime = new NodeRuntime[TaskEnv](nodeConf, kamonConf, id, scheduler)

      // Run reader layer with initial state
      _ <- runtime.main.run(NodeCallCtx.init)
    } yield ()
  }

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

  def setupNodeProgram[F[_]: Monixable: Concurrent: Parallel: ContextShift: Time: TransportLayer: LocalEnvironment: Log: EventLog: Metrics](
      rpConnections: ConnectionsCell[F],
      rpConfAsk: ApplicativeAsk[F, RPConf],
      commUtil: CommUtil[F],
      blockRetriever: BlockRetriever[F],
      conf: NodeConf,
      dagConfig: BlockDagFileStorage.Config,
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
      // In memory state for last approved block
      lab <- LastApprovedBlock.of[F]

      span = if (conf.metrics.zipkin)
        diagnostics.effects
          .span(conf.protocolServer.networkId, conf.protocolServer.host.getOrElse("-"))
      else Span.noop[F]

      // Key-value store manager / manages LMDB databases
      casperStoreManager <- RNodeKeyValueStoreManager(conf.storage.dataDir)

      // Block storage
      blockStore <- {
        implicit val kvm = casperStoreManager
        // Check if old file based block store exists
        val oldBlockStoreExists = blockstorePath.resolve("storage").toFile.exists
        // TODO: remove file based block store in future releases
        def oldStorage = {
          val blockstoreEnv = Context.env(blockstorePath, conf.storage.lmdbMapSizeBlockstore)
          for {
            blockStore <- FileLMDBIndexBlockStore
                           .create[F](blockstoreEnv, blockstorePath)(
                             Concurrent[F],
                             Sync[F],
                             Log[F],
                             Metrics[F]
                           )
                           .map(_.right.get) // TODO handle errors
          } yield blockStore
        }
        // Start block storage
        if (oldBlockStoreExists) oldStorage else KeyValueBlockStore()
      }
      // Last finalized Block storage
      lastFinalizedStorage <- {
        for {
          lastFinalizedBlockDb   <- casperStoreManager.store("last-finalized-block")
          lastFinalizedIsEmpty   = lastFinalizedBlockDb.iterate(_.isEmpty)
          oldLastFinalizedExists = Sync[F].delay(Files.exists(lastFinalizedPath))
          shouldMigrate          <- lastFinalizedIsEmpty &&^ oldLastFinalizedExists
          lastFinalizedStore     = LastFinalizedKeyValueStorage(lastFinalizedBlockDb)
          _ <- LastFinalizedKeyValueStorage
                .importFromFileStorage(lastFinalizedPath, lastFinalizedStore)
                .whenA(shouldMigrate)
        } yield lastFinalizedStore
      }
      // Block DAG storage
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
      deployStorageAllocation               <- LMDBDeployStorage.make[F](deployStorageConfig).allocated
      (deployStorage, deployStorageCleanup) = deployStorageAllocation
      oracle = {
        implicit val sp = span
        SafetyOracle.cliqueOracle[F]
      }
      lastFinalizedBlockCalculator = {
        implicit val bs = blockStore
        implicit val da = blockDagStorage
        implicit val or = oracle
        implicit val ds = deployStorage
        LastFinalizedBlockCalculator[F](
          conf.casper.faultToleranceThreshold
        )
      }
      estimator = {
        implicit val sp = span
        Estimator[F](conf.casper.maxNumberOfParents, conf.casper.maxParentDepth)
      }
      synchronyConstraintChecker = {
        implicit val bs = blockStore
        implicit val es = estimator
        implicit val sp = span
        SynchronyConstraintChecker[F](
          conf.casper.synchronyConstraintThreshold
        )
      }
      lastFinalizedHeightConstraintChecker = {
        implicit val bs = blockStore
        implicit val lf = lastFinalizedStorage
        LastFinalizedHeightConstraintChecker[F](
          conf.casper.heightConstraintThreshold
        )
      }
      evalRuntime <- {
        implicit val s  = rspaceScheduler
        implicit val sp = span
        Runtime.setupRSpace[F](cliConf.storage, cliConf.size) >>= {
          case (space, replay, _) => Runtime.createWithEmptyCost[F]((space, replay), Seq.empty)
        }
      }
      _ <- Runtime.bootstrapRegistry[F](evalRuntime)
      casperInitialized <- {
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
        } yield (runtime, reporter, hr)
      }
      (casperRuntime, reportingCasper, historyRepo) = casperInitialized
      runtimeManager <- {
        implicit val sp = span
        RuntimeManager.fromRuntime[F](casperRuntime)
      }
      // RNodeStateManager
      stateManagers <- {
        for {
          exporter           <- historyRepo.exporter
          importer           <- historyRepo.importer
          rspaceStateManager = RSpaceStateManagerImpl(exporter, importer)
          blockStateManager  = BlockStateManagerImpl(blockStore, blockDagStorage)
          rnodeStateManager  = RNodeStateManagerImpl(rspaceStateManager, blockStateManager)
        } yield (rnodeStateManager, rspaceStateManager)
      }
      (rnodeStateManager, rspaceStateManager) = stateManagers
      // Engine dynamic reference
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
        implicit val rsm    = rspaceStateManager

        CasperLaunch.of[F](
          conf.casper,
          !conf.protocolClient.disableLfs,
          conf.protocolServer.disableStateExporter
        )
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
      apiServers = {
        implicit val bs = blockStore
        implicit val ec = engineCell
        implicit val or = oracle
        implicit val sp = span
        implicit val sc = synchronyConstraintChecker
        implicit val lh = lastFinalizedHeightConstraintChecker
        implicit val rc = reportingCasper
        NodeRuntime
          .acquireAPIServers[F](
            evalRuntime,
            blockApiLock,
            scheduler,
            conf.apiServer.maxBlocksLimit,
            conf.devMode
          )
      }
      casperLoop = {
        implicit val br = blockRetriever
        for {
          engine <- engineCell.read
          // Fetch dependencies from CasperBuffer
          _ <- engine.withCasper(_.fetchDependencies, ().pure[F])
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
        new WebApiImpl[F](conf.apiServer.maxBlocksLimit, conf.devMode, rnodeStateManager)
      }
      adminWebApi = {
        implicit val ec     = engineCell
        implicit val sp     = span
        implicit val sc     = synchronyConstraintChecker
        implicit val lfhscc = lastFinalizedHeightConstraintChecker
        new AdminWebApiImpl[F](blockApiLock, rnodeStateManager)
      }
    } yield (
      blockStore,
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

  def acquireAPIServers[F[_]: Monixable](
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
      synchronyConstraintChecker: SynchronyConstraintChecker[F],
      lastFinalizedHeightConstraintChecker: LastFinalizedHeightConstraintChecker[F],
      reportingCasper: ReportingCasper[F]
  ): APIServers = {
    implicit val s: Scheduler = scheduler
    val repl                  = ReplGrpcService(runtime, s)
    val deploy =
      DeployGrpcServiceV1(apiMaxBlocksLimit, reportingCasper, devMode)
    val propose = ProposeGrpcServiceV1(blockApiLock)
    APIServers(repl, propose, deploy)
  }
}
