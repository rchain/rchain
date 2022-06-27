package coop.rchain.node.runtime

import cats.data.ReaderT
import cats.effect._
import cats.effect.concurrent.{Deferred, Ref}
import cats.mtl._
import cats.syntax.all._
import cats.{~>, Parallel}
import com.typesafe.config.Config
import coop.rchain.casper._
import coop.rchain.casper.blocks.proposer.{Proposer, ProposerResult}
import coop.rchain.casper.engine.BlockRetriever
import coop.rchain.casper.protocol.CommUtil
import coop.rchain.casper.state.instances.ProposerState
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import coop.rchain.comm.discovery._
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk, RPConfState}
import coop.rchain.comm.rp._
import coop.rchain.comm.transport._
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.monix.Monixable
import coop.rchain.node.api._
import coop.rchain.node.configuration.NodeConf
import coop.rchain.node.effects.{EventConsumer, RchainEvents}
import coop.rchain.node.instances.ProposerInstance
import coop.rchain.node.runtime.NodeCallCtx.NodeCallCtxReader
import coop.rchain.node.runtime.NodeRuntime._
import coop.rchain.node.web.ReportingRoutes.ReportingHttpRoutes
import coop.rchain.node.{diagnostics, effects, NodeEnvironment}
import coop.rchain.shared._
import coop.rchain.shared.syntax._
import coop.rchain.store.KeyValueStoreManager
import fs2.Stream
import fs2.concurrent.Queue
import kamon._
import monix.execution.Scheduler

import scala.concurrent.duration._

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class NodeRuntime[F[_]: Monixable: ConcurrentEffect: Parallel: Timer: ContextShift: LocalEnvironment: Log] private[node] (
    nodeConf: NodeConf,
    kamonConf: Config,
    id: NodeIdentifier,
    scheduler: Scheduler
) {

  // main scheduler for all CPU bounded tasks
  implicit val mainSheduler = scheduler

  // io scheduler for serving low level calls, e.g. for Netty
  private[this] val grpcScheduler =
    Scheduler.cached("grpc-io", 4, 64, reporter = UncaughtExceptionLogger)
  implicit private val logSource: LogSource = LogSource(this.getClass)

  /**
    * Main node entry. It will:
    * 1. set up configurations
    * 2. create instances of typeclasses
    * 3. run the node program.
    */
  // TODO: Resolve scheduler chaos in Runtime, RuntimeManager and CasperPacketHandler
  def main: F[Unit] = {
    for {
      // Fetch local peer node
      local <- WhoAmI
                .fetchLocalPeerNode[F](
                  nodeConf.protocolServer.host,
                  nodeConf.protocolServer.port,
                  nodeConf.peersDiscovery.port,
                  nodeConf.protocolServer.noUpnp,
                  id
                )

      // Create instances of typeclasses
      metrics = diagnostics.effects.metrics[F]
      time    = Time.fromTimer[F]

      transport <- {
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
      peerNodeAsk     = effects.peerNodeAsk[F](rpConfState)
      rpConfAsk       = effects.rpConfAsk[F](rpConfState)
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
          nodeConf.protocolClient.networkTimeout
        )
      }

      kademliaStore = {
        implicit val k = kademliaRPC
        implicit val m = metrics
        effects.kademliaStore(id)
      }

      _             <- initPeer.fold(().pure[F])(p => kademliaStore.updateLastSeen(p))
      nodeDiscovery = effects.nodeDiscovery(id)(Sync[F], kademliaStore, kademliaRPC)

      eventBus <- RchainEvents[F]

      result <- {
        implicit val tr = transport
        implicit val ti = time
        implicit val me = metrics
        implicit val nd = nodeDiscovery
        Setup.setupNodeProgram[F](
          rpConnections,
          rpConfAsk,
          commUtil,
          blockRetriever,
          nodeConf,
          eventBus
        )
      }
      (
        routingMessageQueue,
        apiServers,
        casperLoop,
        casperLaunch,
        reportingHTTPRoutes,
        webApi,
        adminWebApi,
        proposerOpt,
        proposerQueue,
        proposerStateRefOpt,
        blockProcessorStream
      ) = result

      // Build main program
      program = {
        implicit val tr = transport
        implicit val cn = rpConfAsk
        implicit val cc = rpConnections
        implicit val cs = rpConfState
        implicit val pn = peerNodeAsk
        implicit val ks = kademliaStore
        implicit val nd = nodeDiscovery
        implicit val eb = eventBus
        implicit val mt = metrics
        implicit val ti = time
        nodeProgram(
          apiServers,
          casperLoop,
          reportingHTTPRoutes,
          webApi,
          adminWebApi,
          proposerOpt,
          proposerQueue,
          proposerStateRefOpt,
          blockProcessorStream,
          routingMessageQueue
        )
      }

      // Run main program and casper launch concurrently
      messageProcess = Stream.eval(casperLaunch)
      mainProgram    = Stream.eval(handleUnrecoverableErrors(program))
      delay          = Stream.eval(Timer[F].sleep(3.seconds))

      // TODO: implement cancellation with proper shutdown
      _ <- (mainProgram concurrently (delay ++ messageProcess)).compile.drain
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

  private def nodeProgram(
      apiServers: APIServers,
      casperLoop: CasperLoop[F],
      reportingRoutes: ReportingHttpRoutes[F],
      webApi: WebApi[F],
      adminWebApi: AdminWebApi[F],
      proposer: Option[Proposer[F]],
      proposeRequestsQueue: Queue[F, (Boolean, Deferred[F, ProposerResult])],
      proposerStateRefOpt: Option[Ref[F, ProposerState[F]]],
      blockProcessorStream: Stream[F, Unit],
      routingMessageQueue: Queue[F, RoutingMessage]
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
        _ <- time.sleep(1.second)
        _ <- ConnectionsCell[F].get
              .map(_.isEmpty)
              .ifM(waitForFirstConnection, ().pure[F])
      } yield ()

    for {
      _       <- info
      local   <- peerNodeAsk.ask
      address = local.toAddress
      host    = local.endpoint.host
      servers <- ServersInstances.build(
                  apiServers,
                  reportingRoutes,
                  webApi,
                  adminWebApi,
                  HandleMessages.handle[F](_, routingMessageQueue),
                  blob => routingMessageQueue.enqueue1(RoutingMessage(blob.sender, blob.packet)),
                  host,
                  address,
                  nodeConf,
                  kamonConf,
                  grpcScheduler
                )

      nodeDiscoveryStream    = fs2.Stream.eval(nodeDiscoveryLoop).repeat
      clearConnectionsStream = fs2.Stream.eval(clearConnectionsLoop).repeat
      connectivityStream = fs2
        .Stream(
          nodeDiscoveryStream,
          clearConnectionsStream,
          servers.kademliaServer,
          servers.transportServer
        )
        .parJoinUnbounded

      waitForFirstConnectionStream = if (nodeConf.standalone) fs2.Stream.empty
      else
        fs2.Stream.eval(
          Log[F].info(s"Waiting for first connection.") >> waitForFirstConnection
        )

      casperLoopStream = fs2.Stream.eval(casperLoop).repeat

      proposerStream = if (proposer.isDefined)
        ProposerInstance
          .create[F](proposeRequestsQueue, proposer.get, proposerStateRefOpt.get)
      else fs2.Stream.empty

      serverStream = fs2
        .Stream(
          servers.externalApiServer,
          servers.internalApiServer,
          servers.httpServer,
          servers.adminHttpServer,
          blockProcessorStream,
          proposerStream,
          casperLoopStream
        )
        .parJoinUnbounded

      // run all streams in parallel, but start server streams after node sees some peers
      node = fs2
        .Stream(
          connectivityStream,
          waitForFirstConnectionStream ++ serverStream
        )
        .parJoinUnbounded

      _ <- node.compile.drain
    } yield ()
  }

  def addShutdownHook(
      servers: ServersInstances[F]
  ): F[Unit] =
    Sync[F].delay(sys.addShutdownHook(clearResources(servers))).void

  def clearResources(
      servers: ServersInstances[F]
  ): Unit = {
    val shutdown = for {
      _ <- Sync[F].delay(Kamon.stopAllReporters())
      _ <- Log[F].info("Bringing BlockStore down ...")
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

}

object NodeRuntime {
  type LocalEnvironment[F[_]] = ApplicativeLocal[F, NodeCallCtx]

  type CasperLoop[F[_]] = F[Unit]

  def start[F[_]: Monixable: ConcurrentEffect: Parallel: ContextShift: Timer: Log](
      nodeConf: NodeConf,
      kamonConf: Config
  )(implicit scheduler: Scheduler): F[Unit] = {

    val nodeCallCtxReader: NodeCallCtxReader[F] = NodeCallCtxReader[F]()
    import nodeCallCtxReader._

    /**
      * ReaderT instances for NodeRuntime dependencies. Implementations for Log and EventLog are created "manually"
      * although they can be generated with cats.tagless @autoFunctorK macros but support is missing for IntelliJ.
      * https://github.com/typelevel/cats-tagless/issues/60 (Cheers, Marcin!!)
      */
    implicit val lg: Log[ReaderNodeCallCtx]       = Log[F].mapK(effToEnv)
    implicit val tm: Timer[ReaderNodeCallCtx]     = Timer[F].mapK(effToEnv)
    implicit val mn: Monixable[ReaderNodeCallCtx] = Monixable[F].mapK(effToEnv, NodeCallCtx.init)

    for {
      id <- NodeEnvironment.create[F](nodeConf)

      // Create NodeRuntime instance
      runtime = new NodeRuntime[ReaderNodeCallCtx](nodeConf, kamonConf, id, scheduler)

      // Run reader layer with initial state
      _ <- runtime.main.run(NodeCallCtx.init)
    } yield ()
  }
}
