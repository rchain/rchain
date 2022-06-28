package coop.rchain.node.runtime

import cats.Parallel
import cats.effect._
import cats.effect.concurrent.Ref
import cats.mtl._
import cats.syntax.all._
import com.typesafe.config.Config
import coop.rchain.casper._
import coop.rchain.casper.engine.BlockRetriever
import coop.rchain.casper.protocol.CommUtil
import coop.rchain.comm._
import coop.rchain.comm.discovery._
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfState}
import coop.rchain.comm.rp._
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.monix.Monixable
import coop.rchain.node.configuration.NodeConf
import coop.rchain.node.runtime.NodeCallCtx.NodeCallCtxReader
import coop.rchain.node.runtime.NodeRuntime._
import coop.rchain.node.{diagnostics, effects, NodeEnvironment}
import coop.rchain.shared._
import fs2.Stream
import monix.execution.Scheduler

import scala.concurrent.duration._

object NodeRuntime {
  type LocalEnvironment[F[_]] = ApplicativeLocal[F, NodeCallCtx]

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

  /**
    * Runs dynamic IP check and update local state [[RPConf]].
    */
  def dynamicIpCheck[F[_]: Sync: ConnectionsCell: RPConfState: Log](
      nodeConf: NodeConf
  ): F[Unit] =
    for {
      local         <- RPConfState[F].get.map(_.local)
      protocolPort  = nodeConf.protocolServer.port
      discoveryPort = nodeConf.peersDiscovery.port
      newLocal      <- WhoAmI.checkLocalPeerNode[F](protocolPort, discoveryPort, local)
      _ <- newLocal.traverse_ { peer =>
            Connect.resetConnections[F] *> RPConfState[F].modify(_.copy(local = peer))
          }
    } yield ()
}

class NodeRuntime[F[_]: Monixable: ConcurrentEffect: Parallel: Timer: ContextShift: LocalEnvironment: Log] private[node] (
    nodeConf: NodeConf,
    kamonConf: Config,
    id: NodeIdentifier,
    scheduler: Scheduler
) {
  // Main scheduler for all CPU bounded tasks
  implicit val mainSheduler = scheduler

  // TODO: revise use of schedulers for gRPC
  private[this] val grpcScheduler =
    Scheduler.cached("grpc-io", 4, 64, reporter = UncaughtExceptionLogger)
  implicit private val logSource: LogSource = LogSource(this.getClass)

  /**
    * Main node entry. It will:
    * 1. set up configurations
    * 2. create instances of typeclasses
    * 3. run the node program.
    */
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
      rpConfAsk       = effects.rpConfAsk[F](rpConfState)
      requestedBlocks <- Ref.of[F, Map[BlockHash, engine.BlockRetriever.RequestState]](Map.empty)

      commUtil = {
        implicit val (tr, cn, cc) = (transport, rpConfAsk, rpConnections)
        CommUtil.of[F]
      }

      blockRetriever = {
        implicit val (t, r, c, s, m) = (transport, rpConfAsk, commUtil, requestedBlocks, metrics)
        BlockRetriever.of[F]
      }

      // Node discovery service (Kademlia)
      kademliaRPC = {
        implicit val (p, g, m) = (rpConfAsk, grpcScheduler, metrics)
        effects.kademliaRPC(
          nodeConf.protocolServer.networkId,
          nodeConf.protocolClient.networkTimeout
        )
      }

      kademliaStore = {
        implicit val (k, m) = (kademliaRPC, metrics)
        effects.kademliaStore(id)
      }

      _ <- initPeer.traverse_(kademliaStore.updateLastSeen)
      nodeDiscovery = {
        implicit val (ks, kr) = (kademliaStore, kademliaRPC)
        effects.nodeDiscovery(id)
      }

      // Running node as a Stream
      result <- {
        implicit val (t, n, m) = (transport, nodeDiscovery, metrics)
        Setup.setupNodeProgram[F](
          rpConnections,
          rpConfAsk,
          commUtil,
          blockRetriever,
          nodeConf
        )
      }
      (nodeLaunchStream, routingMessageQueue, grpcServices, webApi, adminWebApi, reportingRoutes) = result

      // Build network resources
      networkResources = {
        implicit val (tr, ca, cc) = (transport, rpConfAsk, rpConnections)
        implicit val (ks, nd, me) = (kademliaStore, nodeDiscovery, metrics)
        NetworkServers.create(
          routingMessageQueue,
          grpcServices,
          webApi,
          adminWebApi,
          reportingRoutes,
          nodeConf,
          kamonConf,
          grpcScheduler
        )
      }

      // Maintain network connections (discover peers)
      discoveryConnect = {
        implicit val (t, n, c, r, m) = (transport, nodeDiscovery, rpConnections, rpConfAsk, metrics)
        for {
          _ <- NodeDiscovery[F].discover
          _ <- Connect.findAndConnect[F](Connect.connect[F])
          _ <- Timer[F].sleep(20.seconds)
        } yield ()
      }

      // Maintain network connections (clear stale peers)
      // TODO: check for ways to more precise fine unavailable peers
      clearConnections = {
        implicit val (tl, cn, cs) = (transport, rpConnections, rpConfState)
        implicit val (rp, me)     = (rpConfAsk, metrics)
        for {
          _ <- dynamicIpCheck(nodeConf).whenA(nodeConf.protocolServer.dynamicIp)
          _ <- Connect.clearConnections[F]
          _ <- Timer[F].sleep(10.minutes)
        } yield ()
      }

      // Node starting log messages
      _ <- Log[F].info(s"Starting stand-alone node.").whenA(nodeConf.standalone)
      _ <- Log[F]
            .info(s"Starting node that will bootstrap from ${nodeConf.protocolClient.bootstrap}")
            .unlessA(nodeConf.standalone)

      // Create main process combined from concurrent streams
      mainProcess = nodeLaunchStream concurrently
        Stream.eval(discoveryConnect).repeat concurrently
        Stream.eval(clearConnections).repeat

      _ <- networkResources
            .onFinalize(Log[F].warn(s"Graceful shutdown, all resources are successfully closed."))
            .use(_ => mainProcess.compile.drain)
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
}
