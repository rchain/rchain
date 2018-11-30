package coop.rchain.node

import scala.concurrent.duration._

import cats._
import cats.data._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.functor._
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.blockstorage.{BlockStore, InMemBlockStore}
import coop.rchain.casper._
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.comm.CasperPacketHandler
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
import coop.rchain.metrics.Metrics
import coop.rchain.node.api._
import coop.rchain.node.configuration.Configuration
import coop.rchain.node.diagnostics._
import coop.rchain.p2p.effects._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.shared._

import kamon._
import kamon.zipkin.ZipkinReporter
import monix.eval.Task
import monix.execution.Scheduler
import org.http4s.server.blaze._

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

  private implicit val logSource: LogSource = LogSource(this.getClass)

  implicit def eiterTrpConfAsk(implicit ev: RPConfAsk[Task]): RPConfAsk[Effect] =
    new EitherTApplicativeAsk[Task, RPConf, CommError]

  import ApplicativeError_._

  /** Configuration */
  private val port              = conf.server.port
  private val kademliaPort      = conf.server.kademliaPort
  private val storagePath       = conf.server.dataDir.resolve("rspace")
  private val casperStoragePath = storagePath.resolve("casper")
  private val storageSize       = conf.server.mapSize
  private val storeType         = conf.server.storeType
  private val defaultTimeout    = FiniteDuration(conf.server.defaultTimeout.toLong, MILLISECONDS) // TODO remove

  case class Servers(
      grpcServerExternal: GrpcServer,
      grpcServerInternal: GrpcServer,
      httpServer: Fiber[Task, Unit]
  )

  def acquireServers(runtime: Runtime)(
      implicit
      nodeDiscovery: NodeDiscovery[Task],
      blockStore: BlockStore[Effect],
      oracle: SafetyOracle[Effect],
      multiParentCasperRef: MultiParentCasperRef[Effect],
      nodeCoreMetrics: NodeMetrics[Task],
      jvmMetrics: JvmMetrics[Task],
      connectionsCell: ConnectionsCell[Task]
  ): Effect[Servers] = {
    implicit val s: Scheduler = scheduler
    for {
      grpcServerExternal <- GrpcServer
                             .acquireExternalServer[Effect](
                               conf.grpcServer.portExternal,
                               conf.server.maxMessageSize,
                               grpcScheduler
                             )
      grpcServerInternal <- GrpcServer
                             .acquireInternalServer(
                               conf.grpcServer.portInternal,
                               conf.server.maxMessageSize,
                               runtime,
                               grpcScheduler
                             )
                             .toEffect

      prometheusReporter = new NewPrometheusReporter()
      prometheusService  = NewPrometheusReporter.service(prometheusReporter)

      httpServerFiber <- BlazeBuilder[Task]
                          .bindHttp(conf.server.httpPort, "0.0.0.0")
                          .mountService(prometheusService, "/metrics")
                          .mountService(VersionInfo.service, "/version")
                          .resource
                          .use(_ => Task.never[Unit])
                          .start
                          .toEffect

      _ <- Task.delay {
            Kamon.addReporter(prometheusReporter)
            Kamon.addReporter(new JmxReporter())
            Kamon.addReporter(new ZipkinReporter())
          }.toEffect
    } yield Servers(grpcServerExternal, grpcServerInternal, httpServerFiber)
  }

  def clearResources(servers: Servers, runtime: Runtime, casperRuntime: Runtime)(
      implicit
      transport: TransportLayer[Task],
      blockStore: BlockStore[Effect],
      peerNodeAsk: PeerNodeAsk[Task]
  ): Unit =
    (for {
      _   <- log.info("Shutting down gRPC servers...")
      _   <- servers.grpcServerExternal.stop
      _   <- servers.grpcServerInternal.stop
      _   <- log.info("Shutting down transport layer, broadcasting DISCONNECT")
      loc <- peerNodeAsk.ask
      msg = ProtocolHelper.disconnect(loc)
      _   <- transport.shutdown(msg)
      _   <- log.info("Shutting down HTTP server....")
      _   <- Task.delay(Kamon.stopAllReporters())
      _   <- servers.httpServer.cancel
      _   <- log.info("Shutting down interpreter runtime ...")
      _   <- runtime.close()
      _   <- log.info("Shutting down Casper runtime ...")
      _   <- casperRuntime.close()
      _   <- log.info("Bringing BlockStore down ...")
      _   <- blockStore.close().value
      _   <- log.info("Goodbye.")
    } yield ()).unsafeRunSync(scheduler)

  def startReportJvmMetrics(
      implicit metrics: Metrics[Task],
      jvmMetrics: JvmMetrics[Task]
  ): Task[Unit] =
    Task.delay {
      import scala.concurrent.duration._
      loopScheduler.scheduleAtFixedRate(3.seconds, 3.second)(
        JvmMetrics.report[Task].unsafeRunSync(scheduler)
      )
    }

  def addShutdownHook(servers: Servers, runtime: Runtime, casperRuntime: Runtime)(
      implicit transport: TransportLayer[Task],
      blockStore: BlockStore[Effect],
      peerNodeAsk: PeerNodeAsk[Task]
  ): Task[Unit] =
    Task.delay(sys.addShutdownHook(clearResources(servers, runtime, casperRuntime)))

  private def exit0: Task[Unit] = Task.delay(System.exit(0))

  private def nodeProgram(runtime: Runtime, casperRuntime: Runtime)(
      implicit
      time: Time[Task],
      rpConfState: RPConfState[Task],
      rpConfAsk: RPConfAsk[Task],
      peerNodeAsk: PeerNodeAsk[Task],
      metrics: Metrics[Task],
      transport: TransportLayer[Task],
      nodeDiscovery: NodeDiscovery[Task],
      rpConnectons: ConnectionsCell[Task],
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
          local    <- peerNodeAsk.ask
          newLocal <- conf.checkLocalPeerNode(local)
          _ <- newLocal.fold(Task.unit) { pn =>
                Connect.resetConnections[Task].flatMap(kp(rpConfState.modify(_.copy(local = pn))))
              }
        } yield ()
      else Task.unit

    val loop: Effect[Unit] =
      for {
        _ <- time.sleep(1.minute).toEffect
        _ <- dynamicIpCheck.toEffect
        _ <- Connect.clearConnections[Effect]
        _ <- Connect.findAndConnect[Effect](Connect.connect[Effect])
      } yield ()

    val casperLoop: Effect[Unit] =
      for {
        casper <- casperConstructor.get
        _      <- casper.fold(().pure[Effect])(_.fetchDependencies)
        _      <- time.sleep(30.seconds).toEffect
      } yield ()

    for {
      _       <- info
      local   <- peerNodeAsk.ask.toEffect
      host    = local.endpoint.host
      servers <- acquireServers(runtime)
      _       <- addShutdownHook(servers, runtime, casperRuntime).toEffect
      _       <- servers.grpcServerExternal.start.toEffect
      _ <- Log[Effect].info(
            s"gRPC external server started at $host:${servers.grpcServerExternal.port}"
          )
      _ <- servers.grpcServerInternal.start.toEffect
      _ <- Log[Effect].info(
            s"gRPC internal server started at $host:${servers.grpcServerInternal.port}"
          )
      _ <- startReportJvmMetrics.toEffect

      _ <- TransportLayer[Effect].receive(
            pm => HandleMessages.handle[Effect](pm, defaultTimeout),
            blob => packetHandler.handlePacket(blob.sender, blob.packet).as(())
          )
      _       <- NodeDiscovery[Task].discover.executeOn(loopScheduler).start.toEffect
      _       <- Task.defer(casperLoop.forever.value).executeOn(loopScheduler).start.toEffect
      address = s"rnode://$id@$host?protocol=$port&discovery=$kademliaPort"
      _       <- Log[Effect].info(s"Listening for traffic on $address.")
      _       <- EitherT(Task.defer(loop.forever.value).executeOn(loopScheduler))
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

  private def syncEffect = cats.effect.Sync.catsEitherTSync[Task, CommError]

  private val rpClearConnConf = ClearConnetionsConf(
    conf.server.maxNumOfConnections,
    numOfConnectionsPinged = 10
  ) // TODO read from conf

  private def rpConf(local: PeerNode, bootstrapNode: Option[PeerNode]) =
    RPConf(local, bootstrapNode, defaultTimeout, rpClearConnConf)

  /**
    * Main node entry. It will:
    * 1. set up configurations
    * 2. create instances of typeclasses
    * 3. run the node program.
    */
  // TODO: Resolve scheduler chaos in Runtime, RuntimeManager and CasperPacketHandler
  val main: Effect[Unit] = for {
    // 1. fetch local peer node
    local <- conf.fetchLocalPeerNode(id).toEffect

    // 2. set up configurations
    defaultTimeout = conf.server.defaultTimeout.millis

    // 3. create instances of typeclasses
    initPeer             = if (conf.server.standalone) None else Some(conf.server.bootstrap)
    rpConfState          = effects.rpConfState(rpConf(local, initPeer))
    rpConfAsk            = effects.rpConfAsk(rpConfState)
    peerNodeAsk          = effects.peerNodeAsk(rpConfState)
    rpConnections        <- effects.rpConnections.toEffect
    kademliaConnections  <- CachedConnections[Task, KademliaConnTag].toEffect
    tcpConnections       <- CachedConnections[Task, TcpConnTag].toEffect
    time                 = effects.time
    timerTask            = Task.timer
    metrics              = diagnostics.metrics[Task]
    multiParentCasperRef <- MultiParentCasperRef.of[Effect]
    lab                  <- LastApprovedBlock.of[Task].toEffect
    labEff               = LastApprovedBlock.eitherTLastApprovedBlock[CommError, Task](Monad[Task], lab)
    transport = effects.tcpTransportLayer(
      port,
      conf.tls.certificate,
      conf.tls.key,
      conf.server.maxMessageSize,
      conf.server.dataDir.resolve("tmp").resolve("comm")
    )(grpcScheduler, log, tcpConnections)
    kademliaRPC = effects.kademliaRPC(kademliaPort, defaultTimeout)(
      grpcScheduler,
      peerNodeAsk,
      metrics,
      log,
      kademliaConnections
    )
    nodeDiscovery <- effects
                      .nodeDiscovery(id, defaultTimeout)(initPeer)(log, time, metrics, kademliaRPC)
                      .toEffect
    // TODO: This change is temporary until itegulov's BlockStore implementation is in
    blockMap <- Ref.of[Effect, Map[BlockHash, BlockMessage]](Map.empty[BlockHash, BlockMessage])
    blockStore = InMemBlockStore.create[Effect](
      syncEffect,
      blockMap,
      Metrics.eitherT(Monad[Task], metrics)
    )
    _       <- blockStore.clear() // TODO: Replace with a proper casper init when it's available
    oracle  = SafetyOracle.turanOracle[Effect](Monad[Effect])
    runtime = Runtime.create(storagePath, storageSize, storeType)(rspaceScheduler)
    _ <- Runtime
          .injectEmptyRegistryRoot[Task](runtime.space, runtime.replaySpace)
          .toEffect
    casperRuntime  = Runtime.create(casperStoragePath, storageSize, storeType)(rspaceScheduler)
    runtimeManager = RuntimeManager.fromRuntime(casperRuntime)(scheduler)
    casperPacketHandler <- CasperPacketHandler
                            .of[Effect](conf.casper, defaultTimeout, runtimeManager, _.value)(
                              labEff,
                              Metrics.eitherT(Monad[Task], metrics),
                              blockStore,
                              Cell.eitherTCell(Monad[Task], rpConnections),
                              NodeDiscovery.eitherTNodeDiscovery(Monad[Task], nodeDiscovery),
                              TransportLayer.eitherTTransportLayer(Monad[Task], log, transport),
                              ErrorHandler[Effect],
                              eiterTrpConfAsk(rpConfAsk),
                              oracle,
                              Capture[Effect],
                              Sync[Effect],
                              Time.eitherTTime(Monad[Task], time),
                              Log.eitherTLog(Monad[Task], log),
                              multiParentCasperRef,
                              scheduler
                            )
    packetHandler = PacketHandler.pf[Effect](casperPacketHandler.handle)(
      Applicative[Effect],
      Log.eitherTLog(Monad[Task], log),
      ErrorHandler[Effect]
    )
    nodeCoreMetrics = diagnostics.nodeCoreMetrics[Task]
    jvmMetrics      = diagnostics.jvmMetrics[Task]

    // 4. run the node program.
    program = nodeProgram(runtime, casperRuntime)(
      time,
      rpConfState,
      rpConfAsk,
      peerNodeAsk,
      metrics,
      transport,
      nodeDiscovery,
      rpConnections,
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
