package coop.rchain.node.runtime

import cats.data.ReaderT
import cats.effect._
import cats.effect.concurrent.Ref
import cats.mtl._
import cats.syntax.all._
import cats.{~>, Parallel}
import com.typesafe.config.Config
import coop.rchain.blockstorage.dag.state.BlockDagState
import coop.rchain.casper.engine
import coop.rchain.casper.engine.BlockRetriever
import coop.rchain.casper.util.comm._
import coop.rchain.casper.processing.MessageProcessor.MessageProcessingStream
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
import coop.rchain.node.instances.ProposerInstance.BlockProposeStream
import coop.rchain.node.web.ReportingRoutes.ReportingHttpRoutes
import coop.rchain.node.{diagnostics, effects, NodeEnvironment}
import coop.rchain.p2p.effects._
import coop.rchain.shared._
import coop.rchain.shared.syntax._
import coop.rchain.store.KeyValueStoreManager
import kamon._
import monix.execution.Scheduler
import NodeRuntime._

import scala.concurrent.duration._

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class NodeRuntime[F[_]: Monixable: ConcurrentEffect: Parallel: Timer: ContextShift: LocalEnvironment: Log: EventLog] private[node] (
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
        packetHandler,
        blockProcessingStream,
        apiServers,
        casperLoop,
        updateForkChoiceLoop,
        engineInit,
        casperLaunch,
        reportingHTTPRoutes,
        webApi,
        adminWebApi,
        proposerStream
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
        implicit val ph = packetHandler
        implicit val eb = eventBus
        implicit val mt = metrics
        implicit val ti = time
        nodeProgram(
          apiServers,
          casperLoop,
          updateForkChoiceLoop,
          engineInit,
          reportingHTTPRoutes,
          webApi,
          adminWebApi,
          blockProcessingStream,
          proposerStream
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

  private def nodeProgram(
      apiServers: APIServers,
      casperLoop: CasperLoop[F],
      updateForkChoiceLoop: CasperLoop[F],
      engineInit: EngineInit[F],
      reportingRoutes: ReportingHttpRoutes[F],
      webApi: WebApi[F],
      adminWebApi: AdminWebApi[F],
      blockProcessingStream: MessageProcessingStream[F, BlockDagState],
      proposerStream: BlockProposeStream[F]
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
        _ <- time.sleep(1.second)
        _ <- ConnectionsCell[F].read
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
                  HandleMessages.handle[F](_),
                  blob => packetHandler.handlePacket(blob.sender, blob.packet),
                  host,
                  address,
                  nodeConf,
                  kamonConf,
                  grpcScheduler
                )
      //_ <- addShutdownHook(servers, runtimeCleanup, blockStore)

      _ <- EventLog[F].publish(Event.NodeStarted(address))

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

      engineInitStream = fs2.Stream.eval(engineInit)

      casperLoopStream = fs2.Stream.eval(casperLoop).repeat

      updateForkChoiceLoopStream = fs2.Stream.eval(updateForkChoiceLoop).repeat

//      serverStream = fs2
//        .Stream(
//          servers.externalApiServer,
//          servers.internalApiServer,
//          servers.httpServer,
//          servers.adminHttpServer,
//          engineInitStream,
//          casperLoopStream,
//          updateForkChoiceLoopStream
//        )
//        .parJoinUnbounded

      // run all streams in parallel, but start server streams after node sees some peers
      node = fs2
        .Stream(
          connectivityStream,
          blockProcessingStream,
          proposerStream,
          waitForFirstConnectionStream,
          servers.externalApiServer,
          servers.internalApiServer,
          servers.httpServer,
          servers.adminHttpServer,
          engineInitStream,
          casperLoopStream,
          updateForkChoiceLoopStream
        )
        .parJoinUnbounded

      _ <- node.compile.drain
    } yield ()
  }

  def addShutdownHook(
      servers: ServersInstances[F],
      runtimeCleanup: Cleanup[F]
  ): F[Unit] =
    Sync[F].delay(sys.addShutdownHook(clearResources(servers, runtimeCleanup))).void

  def clearResources(
      servers: ServersInstances[F],
      runtimeCleanup: Cleanup[F]
  ): Unit = {
    val shutdown = for {
      _ <- Sync[F].delay(Kamon.stopAllReporters())
      _ <- runtimeCleanup.close()
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
  type EngineInit[F[_]] = F[Unit]

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
      * `runCancelable` and `runAsync` are newly provided.
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
      casperStoreManager: KeyValueStoreManager[F]
  ): Cleanup[F] =
    new Cleanup[F] {
      override def close(): F[Unit] =
        for {
          _ <- Log[F].info("Shutting down Casper store manager ...")
          _ <- casperStoreManager.shutdown
        } yield ()
    }
}
