package coop.rchain.node.runtime

import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{Concurrent, ContextShift, Timer}
import cats.mtl.ApplicativeAsk
import cats.syntax.all._
import cats.{Parallel, Show}
import coop.rchain.blockstorage.{approvedStore, BlockStore}
import coop.rchain.casper._
import coop.rchain.casper.api.{BlockApiImpl, BlockReportApi}
import coop.rchain.casper.blocks.proposer.{Proposer, ProposerResult}
import coop.rchain.casper.blocks.{BlockProcessor, BlockReceiver, BlockReceiverState, BlockRetriever}
import coop.rchain.casper.dag.BlockDagKeyValueStorage
import coop.rchain.casper.engine.{NodeLaunch, PeerMessage}
import coop.rchain.casper.protocol.{toCasperMessageProto, BlockMessage, CasperMessage, CommUtil}
import coop.rchain.casper.reporting.{ReportStore, ReportingCasper}
import coop.rchain.casper.rholang.{BlockRandomSeed, RuntimeManager}
import coop.rchain.casper.state.instances.{BlockStateManagerImpl, ProposerState}
import coop.rchain.casper.syntax._
import coop.rchain.comm.RoutingMessage
import coop.rchain.comm.discovery.NodeDiscovery
import coop.rchain.comm.rp.Connect.ConnectionsCell
import coop.rchain.comm.rp.RPConf
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.crypto.PrivateKey
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Par
import coop.rchain.models.syntax.modelsSyntaxByteString
import coop.rchain.monix.Monixable
import coop.rchain.node.api.AdminWebApi.AdminWebApiImpl
import coop.rchain.node.api.WebApi.WebApiImpl
import coop.rchain.node.api.{AdminWebApi, WebApi}
import coop.rchain.node.configuration.NodeConf
import coop.rchain.node.diagnostics
import coop.rchain.node.instances.ProposerInstance
import coop.rchain.node.runtime.NodeRuntime._
import coop.rchain.node.state.instances.RNodeStateManagerImpl
import coop.rchain.node.web.ReportingRoutes.ReportingHttpRoutes
import coop.rchain.node.web.{ReportingRoutes, Transaction}
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rspace.state.instances.RSpaceStateManagerImpl
import coop.rchain.rspace.syntax._
import coop.rchain.shared._
import coop.rchain.shared.syntax._
import coop.rchain.store.KeyValueStoreManager
import fs2.Stream
import fs2.concurrent.Queue
import monix.execution.Scheduler

object Setup {
  def setupNodeProgram[F[_]: Monixable: Concurrent: Parallel: ContextShift: Timer: LocalEnvironment: TransportLayer: NodeDiscovery: Log: Metrics](
      storeManager: KeyValueStoreManager[F],
      rpConnections: ConnectionsCell[F],
      rpConfAsk: ApplicativeAsk[F, RPConf],
      commUtil: CommUtil[F],
      blockRetriever: BlockRetriever[F],
      conf: NodeConf
  )(implicit mainScheduler: Scheduler): F[
    (
        Stream[F, Unit], // Node startup process (protocol messages handling)
        Queue[F, RoutingMessage],
        GrpcServices,
        WebApi[F],
        AdminWebApi[F],
        ReportingHttpRoutes[F]
    )
  ] = {
    // TODO: temporary until Time is removed completely
    //  https://github.com/rchain/rchain/issues/3730
    implicit val time = Time.fromTimer(Timer[F])

    for {
      // Block execution tracker
      executionTracker <- StatefulExecutionTracker[F]

      // Block storage
      blockStore    <- BlockStore(storeManager)
      approvedStore <- approvedStore.create(storeManager)

      // Block DAG storage
      blockDagStorage <- BlockDagKeyValueStorage.create[F](storeManager)

      // Create metrics if enabled
      span = if (conf.metrics.zipkin)
        diagnostics.effects
          .span(conf.protocolServer.networkId, conf.protocolServer.host.getOrElse("-"))
      else Span.noop[F]

      // Runtime for `rnode eval`
      evalRuntime <- {
        implicit val sp = span
        storeManager.evalStores.flatMap(RhoRuntime.createRuntime[F](_, Par()))
      }

      // Runtime manager (play and replay runtimes)
      runtimeManagerWithHistory <- {
        implicit val sp = span
        for {
          rStores    <- storeManager.rSpaceStores
          mergeStore <- RuntimeManager.mergeableStore(storeManager)
          rm <- RuntimeManager
                 .createWithHistory[F](
                   rStores,
                   mergeStore,
                   BlockRandomSeed.nonNegativeMergeableTagName(conf.casper.shardName),
                   executionTracker
                 )
        } yield rm
      }
      (runtimeManager, historyRepo) = runtimeManagerWithHistory

      // Reporting runtime
      reportingRuntime <- {
        implicit val (bd, sp) = (blockDagStorage, span)
        if (conf.apiServer.enableReporting) {
          // In reporting replay channels map is not needed
          storeManager.rSpaceStores.map(ReportingCasper.rhoReporter(_, conf.casper.shardName))
        } else
          ReportingCasper.noop.pure[F]
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

      // Load validator private key if specified
      validatorIdentityOpt <- ValidatorIdentity.fromPrivateKeyWithLogging[F](
                               conf.casper.validatorPrivateKey
                             )

      // Proposer instance
      proposer = validatorIdentityOpt.map { validatorIdentity =>
        implicit val (bs, bd)     = (blockStore, blockDagStorage)
        implicit val (rm, cu, sp) = (runtimeManager, commUtil, span)
        val dummyDeployerKeyOpt   = conf.dev.deployerPrivateKey
        val dummyDeployerKey      = dummyDeployerKeyOpt.flatMap(Base16.decode(_)).map(PrivateKey(_))

        // TODO make term for dummy deploy configurable
        Proposer[F](
          validatorIdentity,
          conf.casper.shardName,
          conf.casper.minPhloPrice,
          conf.casper.genesisBlockData.epochLength,
          dummyDeployerKey.map((_, "Nil"))
        )
      }

      // Propose request is a tuple - Casper, async flag and deferred proposer result that will be resolved by proposer
      proposerQueue <- Queue.unbounded[F, (Boolean, Deferred[F, ProposerResult])]
      triggerProposeFOpt: Option[ProposeFunction[F]] = if (proposer.isDefined)
        Some(
          (isAsync: Boolean) =>
            for {
              d <- Deferred[F, ProposerResult]
              _ <- proposerQueue.enqueue1((true, d))
              r <- d.get
            } yield r
        )
      else none[ProposeFunction[F]]
      proposerStateRefOpt <- triggerProposeFOpt.traverse(_ => Ref.of(ProposerState[F]()))

      // Queue of received blocks from gRPC API
      incomingBlocksQueue <- Queue.unbounded[F, BlockMessage]
      // Stream of blocks received over the network
      incomingBlockStream = incomingBlocksQueue.dequeue
      // Queue of validated blocks, result of block processor
      validatedBlocksQueue <- Queue.unbounded[F, BlockMessage]
      // Validated blocks stream with auto-propose trigger
      validatedBlocksStream = validatedBlocksQueue.dequeue.evalTap { _ =>
        // If auto-propose is enabled, trigger propose immediately after block finished validation
        triggerProposeFOpt.traverse(_(true)) whenA conf.autopropose
      }
      // Queue of network (protocol) messages
      routingMessageQueue <- Queue.unbounded[F, RoutingMessage]

      // Block receiver, process incoming blocks and order by validated dependencies
      blockReceiverState <- {
        implicit val hashShow = Show.show[BlockHash](_.toHexString)
        Ref.of(BlockReceiverState[BlockHash])
      }
      blockReceiverStream <- {
        implicit val (bs, bd, br) = (blockStore, blockDagStorage, blockRetriever)
        BlockReceiver[F](
          blockReceiverState,
          incomingBlockStream,
          validatedBlocksStream,
          conf.casper.shardName
        )
      }
      // Blocks from receiver with fork-choice tips request on idle
      // TODO: instead of idle timeout more precise trigger can be when peers connect
      blockReceiverFCTStream = blockReceiverStream.evalOnIdle(
        commUtil.sendForkChoiceTipRequest,
        conf.casper.forkChoiceStaleThreshold
      )

      // Block processor (validation of blocks)
      blockProcessorInputBlocksStream = {
        import coop.rchain.blockstorage.syntax._
        blockReceiverFCTStream.evalMap(blockStore.getUnsafe)
      }
      blockProcessorStream = {
        implicit val (rm, sp)     = (runtimeManager, span)
        implicit val (bs, bd, cu) = (blockStore, blockDagStorage, commUtil)
        BlockProcessor[F](
          blockProcessorInputBlocksStream,
          validatedBlocksQueue,
          conf.casper.shardName,
          conf.casper.minPhloPrice
        )
      }

      // Query for network information (address, peers, nodes)
      getNetworkStatus = for {
        address <- rpConfAsk.ask
        peers   <- rpConnections.get
        nodes   <- NodeDiscovery[F].peers
      } yield (address.local, peers, nodes)

      // Block API
      blockApi <- {
        implicit val (bds, bs) = (blockDagStorage, blockStore)
        implicit val rm        = runtimeManager
        implicit val sp        = span
        val isNodeReadOnly     = conf.casper.validatorPrivateKey.isEmpty
        BlockApiImpl[F](
          validatorIdentityOpt,
          conf.protocolServer.networkId,
          conf.casper.shardName,
          conf.casper.minPhloPrice,
          coop.rchain.node.web.VersionInfo.get,
          getNetworkStatus,
          isNodeReadOnly,
          conf.apiServer.maxBlocksLimit,
          conf.devMode,
          triggerProposeFOpt,
          proposerStateRefOpt,
          conf.autopropose,
          executionTracker
        )
      }

      // Report API
      reportingStore <- ReportStore.store[F](storeManager)
      blockReportApi = {
        implicit val bs = blockStore
        BlockReportApi[F](reportingRuntime, reportingStore, validatorIdentityOpt)
      }

      // gRPC services (deploy, propose eval/repl)
      grpcServices = GrpcServices.build[F](blockApi, blockReportApi, evalRuntime)

      // Reporting HTTP routes
      reportingRoutes = ReportingRoutes.service[F](blockReportApi)

      // Transaction API
      transactionAPI = Transaction[F](
        blockReportApi,
        BlockRandomSeed.transferUnforgeable(conf.casper.shardName)
      )
      cacheTransactionAPI <- Transaction.cacheTransactionAPI(transactionAPI, storeManager)

      // Peer message stream
      peerMessageStream = routingMessageQueue
        .dequeueChunk(maxSize = 1)
        .parEvalMapUnorderedProcBounded {
          case RoutingMessage(peer, packet) =>
            toCasperMessageProto(packet).toEither
              .flatMap(CasperMessage.from)
              .map(cm => PeerMessage(peer, cm).some.pure[F])
              .leftMap { err =>
                val msg = s"Could not extract casper message from packet sent by $peer: $err"
                Log[F].warn(msg).as(none[PeerMessage])
              }
              .merge
        }
        .collect { case Some(m) => m }

      // Proposer process stream
      proposerStream = proposer
        .map(ProposerInstance.create[F](proposerQueue, _, proposerStateRefOpt.get))
        .getOrElse(Stream.empty)

      // Infinite loop to trigger request missing dependencies
      requestDependencies = {
        implicit val br = blockRetriever
        for {
          _ <- BlockRetriever[F].requestAll(conf.casper.requestedBlocksTimeout)
          _ <- Time[F].sleep(conf.casper.casperLoopInterval)
        } yield ()
      }

      // Node initialization process, sync LFS to running
      nodeLaunch = {
        implicit val (bs, as, bd) = (blockStore, approvedStore, blockDagStorage)
        implicit val (br, ra, rc) = (blockRetriever, rpConfAsk, rpConnections)
        implicit val (rm, cu)     = (runtimeManager, commUtil)
        implicit val (rsm, sp)    = (rspaceStateManager, span)
        NodeLaunch[F](
          peerMessageStream,
          incomingBlocksQueue,
          conf.casper,
          !conf.protocolClient.disableLfs,
          conf.protocolServer.disableStateExporter,
          validatorIdentityOpt,
          conf.standalone
        )
      }

      // Web API (public and admin)
      webApi      = new WebApiImpl[F](blockApi, cacheTransactionAPI)
      adminWebApi = new AdminWebApiImpl[F](blockApi)

      // Stream represents the whole node process
      nodeProgramStream = Stream.eval(nodeLaunch) concurrently
        proposerStream concurrently
        blockProcessorStream concurrently
        Stream.eval(requestDependencies).repeat
    } yield (
      nodeProgramStream,
      routingMessageQueue,
      grpcServices,
      webApi,
      adminWebApi,
      reportingRoutes
    )
  }
}
