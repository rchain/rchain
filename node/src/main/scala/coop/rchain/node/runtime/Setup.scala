package coop.rchain.node.runtime

import cats.Parallel
import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{Concurrent, ContextShift, Timer}
import cats.mtl.ApplicativeAsk
import cats.syntax.all._
import coop.rchain.blockstorage.syntax._
import coop.rchain.blockstorage.{approvedStore, BlockStore}
import coop.rchain.casper._
import coop.rchain.casper.api.{BlockApiImpl, BlockReportApi}
import coop.rchain.casper.blocks.proposer.{Proposer, ProposerResult}
import coop.rchain.casper.blocks.{BlockProcessor, BlockReceiver, BlockReceiverState}
import coop.rchain.casper.dag.BlockDagKeyValueStorage
import coop.rchain.casper.engine.{BlockRetriever, NodeLaunch, NodeRunning, PeerMessage}
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.protocol.{toCasperMessageProto, BlockMessage, CasperMessage, CommUtil}
import coop.rchain.casper.reporting.{ReportStore, ReportingCasper}
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.state.instances.{BlockStateManagerImpl, ProposerState}
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.comm.RoutingMessage
import coop.rchain.comm.discovery.NodeDiscovery
import coop.rchain.comm.rp.Connect.ConnectionsCell
import coop.rchain.comm.rp.RPConf
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.crypto.PrivateKey
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Par
import coop.rchain.monix.Monixable
import coop.rchain.node.api.AdminWebApi.AdminWebApiImpl
import coop.rchain.node.api.WebApi.WebApiImpl
import coop.rchain.node.api.{AdminWebApi, WebApi}
import coop.rchain.node.configuration.NodeConf
import coop.rchain.node.diagnostics
import coop.rchain.node.runtime.NodeRuntime._
import coop.rchain.node.state.instances.RNodeStateManagerImpl
import coop.rchain.node.web.ReportingRoutes.ReportingHttpRoutes
import coop.rchain.node.web.{ReportingRoutes, Transaction}
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rspace.state.instances.RSpaceStateManagerImpl
import coop.rchain.rspace.syntax._
import coop.rchain.shared._
import coop.rchain.shared.syntax._
import fs2.Stream
import fs2.concurrent.Queue
import monix.execution.Scheduler

object Setup {
  def setupNodeProgram[F[_]: Monixable: Concurrent: Parallel: ContextShift: Time: Timer: TransportLayer: LocalEnvironment: Log: EventLog: Metrics: NodeDiscovery](
      rpConnections: ConnectionsCell[F],
      rpConfAsk: ApplicativeAsk[F, RPConf],
      commUtil: CommUtil[F],
      blockRetriever: BlockRetriever[F],
      conf: NodeConf,
      eventPublisher: EventPublisher[F]
  )(implicit mainScheduler: Scheduler): F[
    (
        Queue[F, RoutingMessage],
        APIServers,
        CasperLoop[F],
        CasperLoop[F],
        F[Unit], // Node startup process (protocol messages handling)
        ReportingHttpRoutes[F],
        WebApi[F],
        AdminWebApi[F],
        Option[Proposer[F]],
        Queue[F, (Boolean, Deferred[F, ProposerResult])],
        Option[Ref[F, ProposerState[F]]],
        Stream[F, Unit]
    )
  ] =
    for {
      // In memory state for last approved block
      lab <- LastApprovedBlock.of[F]

      span = if (conf.metrics.zipkin)
        diagnostics.effects
          .span(conf.protocolServer.networkId, conf.protocolServer.host.getOrElse("-"))
      else Span.noop[F]

      // RNode key-value store manager / manages LMDB databases
      rnodeStoreManager <- RNodeKeyValueStoreManager(conf.storage.dataDir)

      // Block execution tracker
      executionTracker <- StatefulExecutionTracker[F]

      // Block storage
      blockStore    <- BlockStore(rnodeStoreManager)
      approvedStore <- approvedStore.create(rnodeStoreManager)

      // Block DAG storage
      blockDagStorage <- BlockDagKeyValueStorage.create[F](rnodeStoreManager)

      synchronyConstraintChecker = {
        implicit val bs  = blockStore
        implicit val bds = blockDagStorage
        SynchronyConstraintChecker[F]
      }
      lastFinalizedHeightConstraintChecker = {
        implicit val bs  = blockStore
        implicit val bds = blockDagStorage
        LastFinalizedHeightConstraintChecker[F]
      }

      // Runtime for `rnode eval`
      evalRuntime <- {
        implicit val sp = span
        rnodeStoreManager.evalStores.flatMap(RhoRuntime.createRuntime[F](_, Par()))
      }

      // Runtime manager (play and replay runtimes)
      runtimeManagerWithHistory <- {
        implicit val sp = span
        for {
          rStores    <- rnodeStoreManager.rSpaceStores
          mergeStore <- RuntimeManager.mergeableStore(rnodeStoreManager)
          rm <- RuntimeManager
                 .createWithHistory[F](
                   rStores,
                   mergeStore,
                   Genesis.NonNegativeMergeableTagName,
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
          rnodeStoreManager.rSpaceStores.map(ReportingCasper.rhoReporter(_))
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

      casperShardConf = CasperShardConf(
        conf.casper.faultToleranceThreshold,
        conf.casper.shardName,
        conf.casper.finalizationRate,
        conf.casper.maxNumberOfParents,
        conf.casper.maxParentDepth.getOrElse(Int.MaxValue),
        conf.casper.synchronyConstraintThreshold.toFloat,
        conf.casper.heightConstraintThreshold,
        50,
        1,
        1,
        conf.casper.genesisBlockData.bondMinimum,
        conf.casper.genesisBlockData.bondMaximum,
        conf.casper.genesisBlockData.epochLength,
        conf.casper.genesisBlockData.quarantineLength,
        conf.casper.minPhloPrice
      )

      // Load validator private key if specified
      validatorIdentityOpt <- ValidatorIdentity.fromPrivateKeyWithLogging[F](
                               conf.casper.validatorPrivateKey
                             )

      // Proposer instance
      proposer = validatorIdentityOpt.map { validatorIdentity =>
        implicit val (bs, bd)     = (blockStore, blockDagStorage)
        implicit val (br, ep)     = (blockRetriever, eventPublisher)
        implicit val (sc, lh)     = (synchronyConstraintChecker, lastFinalizedHeightConstraintChecker)
        implicit val (rm, cu, sp) = (runtimeManager, commUtil, span)
        val dummyDeployerKeyOpt   = conf.dev.deployerPrivateKey
        val dummyDeployerKey      = dummyDeployerKeyOpt.flatMap(Base16.decode(_)).map(PrivateKey(_))

        // TODO make term for dummy deploy configurable
        Proposer[F](validatorIdentity, casperShardConf, dummyDeployerKey.map((_, "Nil")))
      }

      // Propose request is a tuple - Casper, async flag and deferred proposer result that will be resolved by proposer
      proposerQueue <- Queue.unbounded[F, (Boolean, Deferred[F, ProposerResult])]
      triggerProposeFOpt: Option[ProposeFunction[F]] = if (proposer.isDefined)
        Some(
          (isAsync: Boolean) =>
            for {
              d <- Deferred[F, ProposerResult]
              _ <- proposerQueue.enqueue1((isAsync, d))
              r <- d.get
            } yield r
        )
      else none[ProposeFunction[F]]
      proposerStateRefOpt <- triggerProposeFOpt.traverse(_ => Ref.of(ProposerState[F]()))

      // Queue of received blocks from gRPC API
      incomingBlocksQueue <- Queue.unbounded[F, BlockMessage]
      // Queue of validated blocks, result of block processor
      validatedBlocksQueue <- Queue.unbounded[F, BlockMessage]
      // Validated blocks stream with auto-propose trigger
      validatedBlocksStream = validatedBlocksQueue.dequeue.evalTap { _ =>
        // If auto-propose is enabled, trigger propose immediately after block finished validation
        triggerProposeFOpt.traverse(_(true)) whenA conf.autopropose
      }

      // Block receiver, incoming blocks from peers
      incomingBlockStream = incomingBlocksQueue.dequeue
      blockReceiverState  <- Ref.of(BlockReceiverState[BlockHash])
      blockReceiverStream <- {
        implicit val (bs, bd, br) = (blockStore, blockDagStorage, blockRetriever)
        BlockReceiver[F](
          blockReceiverState,
          incomingBlockStream,
          validatedBlocksStream,
          casperShardConf
        )
      }

      // Block processor (validation of blocks)
      blockProcessorInputBlocksStream = blockReceiverStream.evalMap(blockStore.getUnsafe)
      blockProcessorStream = {
        implicit val (rm, sp)     = (runtimeManager, span)
        implicit val (bs, bd, cu) = (blockStore, blockDagStorage, commUtil)
        BlockProcessor[F](
          blockProcessorInputBlocksStream,
          validatedBlocksQueue,
          MultiParentCasper.getSnapshot[F](casperShardConf)
        )
      }

      // Network packets handler (queue)
      routingMessageQueue <- Queue.unbounded[F, RoutingMessage]
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

      nodeLaunch = {
        implicit val (bs, as, bd) = (blockStore, approvedStore, blockDagStorage)
        implicit val (br, ep)     = (blockRetriever, eventPublisher)
        implicit val (lb, ra, rc) = (lab, rpConfAsk, rpConnections)
        implicit val (sc, lh)     = (synchronyConstraintChecker, lastFinalizedHeightConstraintChecker)
        implicit val (rm, cu)     = (runtimeManager, commUtil)
        implicit val (rsm, sp)    = (rspaceStateManager, span)
        NodeLaunch[F](
          peerMessageStream,
          incomingBlocksQueue,
          conf.casper,
          !conf.protocolClient.disableLfs,
          conf.protocolServer.disableStateExporter,
          validatorIdentityOpt,
          casperShardConf,
          conf.standalone
        )
      }

      // Query for network information (address, peers, nodes)
      getNetworkStatus = for {
        address <- rpConfAsk.ask
        peers   <- rpConnections.read
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
      reportingStore <- ReportStore.store[F](rnodeStoreManager)
      blockReportApi = {
        implicit val bs = blockStore
        BlockReportApi[F](reportingRuntime, reportingStore, validatorIdentityOpt)
      }

      // gRPC services (deploy, propose eval/repl)
      apiServers = APIServers.build[F](blockApi, blockReportApi, evalRuntime)

      // Reporting HTTP routes
      reportingRoutes = ReportingRoutes.service[F](blockReportApi)

      // Transaction API
      transactionAPI = Transaction[F](
        blockReportApi,
        Par(unforgeables = Seq(Transaction.transferUnforgeable))
      )
      cacheTransactionAPI <- Transaction.cacheTransactionAPI(transactionAPI, rnodeStoreManager)

      // Web API (public and admin)
      webApi      = new WebApiImpl[F](blockApi, cacheTransactionAPI)
      adminWebApi = new AdminWebApiImpl[F](blockApi)

      // Infinite loop to trigger request missing dependencies
      casperLoop = {
        implicit val br = blockRetriever
        for {
          // Maintain RequestedBlocks for Casper
          _ <- BlockRetriever[F].requestAll(conf.casper.requestedBlocksTimeout)
          _ <- Time[F].sleep(conf.casper.casperLoopInterval)
        } yield ()
      }

      // Broadcast fork choice tips request if current fork choice is more then `forkChoiceStaleThreshold` minutes old.
      // For why - look at updateForkChoiceTipsIfStuck method description.
      updateForkChoiceLoop = {
        implicit val (bs, cu, bds) = (blockStore, commUtil, blockDagStorage)
        for {
          _ <- Time[F].sleep(conf.casper.forkChoiceCheckIfStaleInterval)
          _ <- NodeRunning.updateForkChoiceTipsIfStuck(conf.casper.forkChoiceStaleThreshold)
        } yield ()
      }

      runtimeCleanup = NodeRuntime.cleanup(
        rnodeStoreManager
      )
    } yield (
      routingMessageQueue,
      apiServers,
      casperLoop,
      updateForkChoiceLoop,
      nodeLaunch,
      reportingRoutes,
      webApi,
      adminWebApi,
      proposer,
      proposerQueue,
      proposerStateRefOpt,
      blockProcessorStream.as(())
    )
}
