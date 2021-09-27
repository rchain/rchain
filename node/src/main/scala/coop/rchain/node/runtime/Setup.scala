package coop.rchain.node.runtime

import cats.Parallel
import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{Concurrent, ContextShift, Sync, Timer}
import cats.mtl.ApplicativeAsk
import cats.syntax.all._
import coop.rchain.blockstorage.KeyValueBlockStore
import coop.rchain.blockstorage.casperbuffer.CasperBufferKeyValueStorage
import coop.rchain.blockstorage.dag.BlockDagKeyValueStorage
import coop.rchain.blockstorage.dag.state.BlockDagState
import coop.rchain.blockstorage.deploy.KeyValueDeployStorage
import coop.rchain.casper._
import coop.rchain.casper.api.BlockReportAPI
import coop.rchain.casper.blocks.proposer.{Proposer, ProposerResult}
import coop.rchain.casper.engine.{BlockRetriever, CasperLaunch, EngineCell, Running}
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.state.instances.{BlockStateManagerImpl, ProposerState}
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.casper.storage.RNodeKeyValueStoreManager.legacyRSpacePathPrefix
import coop.rchain.casper.util.comm.{CasperPacketHandler, CommUtil}
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.v2.processing.MessageProcessor
import coop.rchain.casper.v2.processing.MessageProcessor.MessageProcessingStream
import coop.rchain.comm.rp.Connect.ConnectionsCell
import coop.rchain.comm.rp.RPConf
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.monix.Monixable
import coop.rchain.node.api.AdminWebApi.AdminWebApiImpl
import coop.rchain.node.api.WebApi.WebApiImpl
import coop.rchain.node.api.{AdminWebApi, WebApi}
import coop.rchain.node.blockprocessing.{BlockReceiverImpl, BlockRetrieverImpl, BlockValidatorImpl}
import coop.rchain.node.configuration.NodeConf
import coop.rchain.node.diagnostics
import coop.rchain.node.runtime.NodeRuntime._
import coop.rchain.node.instances.ProposerInstance
import coop.rchain.node.instances.ProposerInstance.BlockProposeStream
import coop.rchain.node.state.instances.RNodeStateManagerImpl
import coop.rchain.node.web.ReportingRoutes
import coop.rchain.node.web.ReportingRoutes.ReportingHttpRoutes
import coop.rchain.p2p.effects.PacketHandler
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rspace.state.instances.RSpaceStateManagerImpl
import coop.rchain.rspace.syntax._
import coop.rchain.shared._
import fs2.concurrent.Queue
import monix.execution.Scheduler

import java.nio.file.Files

object Setup {
  def setupNodeProgram[F[_]: Monixable: Concurrent: Parallel: ContextShift: Time: TransportLayer: LocalEnvironment: Log: EventLog: Metrics: Timer: EventPublisher](
      rpConnections: ConnectionsCell[F],
      rpConfAsk: ApplicativeAsk[F, RPConf],
      commUtil: CommUtil[F],
      blockRetriever: BlockRetriever[F],
      conf: NodeConf
  )(implicit mainScheduler: Scheduler): F[
    (
        PacketHandler[F],
        MessageProcessingStream[F, BlockDagState],
        APIServers,
        CasperLoop[F],
        CasperLoop[F],
        EngineInit[F],
        CasperLaunch[F],
        ReportingHttpRoutes[F],
        WebApi[F],
        AdminWebApi[F],
        BlockProposeStream[F]
    )
  ] = {

    implicit val span: Span[F] =
      if (conf.metrics.zipkin)
        diagnostics.effects
          .span(conf.protocolServer.networkId, conf.protocolServer.host.getOrElse("-"))
      else Span.noop[F]

    for {
      // In memory state for last approved block
      lab <- LastApprovedBlock.of[F]

      // RNode key-value store manager / manages LMDB databases
      oldRSpacePath          = conf.storage.dataDir.resolve(s"$legacyRSpacePathPrefix/history/data.mdb")
      legacyRSpaceDirSupport <- Sync[F].delay(Files.exists(oldRSpacePath))
      rnodeStoreManager      <- RNodeKeyValueStoreManager(conf.storage.dataDir, legacyRSpaceDirSupport)

      // TODO: Old BlockStore migration message, remove after couple of releases from v0.11.0.
      oldBlockStoreExists = conf.storage.dataDir.resolve("blockstore/storage").toFile.exists
      oldBlockStoreMsg    = s"""
       |Old file-based block storage detected (blockstore/storage). To use this version of RNode please first do the migration.
       |Migration should be done with RNode version v0.10.2. More info can be found in PR:
       |https://github.com/rchain/rchain/pull/3342
      """.stripMargin
      _                   <- new Exception(oldBlockStoreMsg).raiseError.whenA(oldBlockStoreExists)

      // Block storage
      blockStore <- KeyValueBlockStore(rnodeStoreManager)

      // Block DAG storage
      blockDagStorage <- BlockDagKeyValueStorage.create[F](rnodeStoreManager)

      // Casper requesting blocks cache
      casperBufferStorage <- CasperBufferKeyValueStorage.create[F](rnodeStoreManager)

      // Deploy storage
      deployStorage <- KeyValueDeployStorage[F](rnodeStoreManager)

      // Runtime for `rnode eval`
      evalRuntime <- rnodeStoreManager.evalStores.flatMap(RhoRuntime.createRuntime[F](_))

      // Runtime manager (play and replay runtimes)
      runtimeManagerWithHistory <- for {
                                    rStores    <- rnodeStoreManager.rSpaceStores
                                    mergeStore <- RuntimeManager.mergeableStore(rnodeStoreManager)
                                    rm         <- RuntimeManager.createWithHistory[F](rStores, mergeStore)
                                  } yield rm
      (runtimeManager, historyRepo) = runtimeManagerWithHistory

      // Reporting runtime
      reportingRuntime <- {
        implicit val (bs, bd) = (blockStore, blockDagStorage)
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

      // Ref holding the latest view on the network and messages
      initValidatedView <- blockDagStorage.getRepresentation.map(_.getPureState)
      initBufferSt      <- casperBufferStorage.toMap
      blockDagStateRef  <- Ref.of[F, BlockDagState](BlockDagState(initBufferSt, initValidatedView))

      // Block processing and validation
      inboundBlocksQueue    <- Queue.unbounded[F, BlockMessage]
      inboundBlocksStream   = inboundBlocksQueue.dequeueChunk(1)
      processBlockInRunning = inboundBlocksQueue.enqueue1 _
      receiverImpl = {
        BlockReceiverImpl[F](
          inboundBlocksStream,
          blockDagStateRef,
          casperBufferStorage,
          blockStore
        )
      }

      validatorImpl <- BlockValidatorImpl(
                        blockDagStateRef,
                        conf.casper,
                        blockStore,
                        blockDagStorage,
                        casperBufferStorage,
                        deployStorage,
                        runtimeManager
                      )
      retrieverImpl = BlockRetrieverImpl(blockRetriever)
      _ <- blockDagStateRef.get
            .map(_.wantedSet.toSet)
            .flatMap(req => Log[F].info(s"Requesting ${req}") >> retrieverImpl.retrieve(req))
      // Block creation
      proposeQueue <- Queue.unbounded[F, (Boolean, Deferred[F, ProposerResult])]
      validatorIdentityOpt <- ValidatorIdentity.fromPrivateKeyWithLogging[F](
                               conf.casper.validatorPrivateKey
                             )
      proposer = validatorIdentityOpt.map { validatorIdentity =>
        val dummyDeployerKeyOpt = conf.dev.deployerPrivateKey
        val dummyDeployerKey =
          if (dummyDeployerKeyOpt.isEmpty) None
          else PrivateKey(Base16.decode(dummyDeployerKeyOpt.get).get).some
        implicit val (bs, bd, ds) = (blockStore, blockDagStorage, deployStorage)
        implicit val br           = (blockRetriever)
        implicit val (rm, cu)     = (runtimeManager, commUtil)
        Proposer[F](validatorIdentity, dummyDeployerKey.map((_, "Nil")), conf.casper)
      }
      triggerProposeFOpt: Option[ProposeFunction[F]] = if (proposer.isDefined)
        Some(
          (isAsync: Boolean) =>
            for {
              d <- Deferred[F, ProposerResult]
              _ <- proposeQueue.enqueue1(isAsync, d)
              r <- d.get
            } yield r
        )
      else none[ProposeFunction[F]]
      proposerStateRefOpt <- triggerProposeFOpt.traverse(_ => Ref.of(ProposerState[F]()))
      proposerStream = if (proposer.isDefined)
        ProposerInstance
          .create[F](proposeQueue, proposer.get, proposerStateRefOpt.get)
      else fs2.Stream.empty

      // Block processing stream. This is used only in Running Engine. Moving it out of Engine is difficult.
      blockProcessingStream = MessageProcessor(receiverImpl, retrieverImpl, validatorImpl).stream(
        triggerProposeFOpt.traverse(_(true)).void
      )

      // Engine dynamic reference
      engineCell <- EngineCell.init[F]
      envVars    = EnvVars.envVars[F]

      casperLaunch = {
        implicit val (bs, bd, ds)         = (blockStore, blockDagStorage, deployStorage)
        implicit val br                   = (blockRetriever)
        implicit val (ec, ev, lb, ra, rc) = (engineCell, envVars, lab, rpConfAsk, rpConnections)
        implicit val (rm, cu)             = (runtimeManager, commUtil)
        implicit val rsm                  = rspaceStateManager
        CasperLaunch.of[F](
          blockDagStateRef,
          if (conf.autopropose) triggerProposeFOpt else none[ProposeFunction[F]],
          conf.casper,
          !conf.protocolClient.disableLfs,
          conf.protocolServer.disableStateExporter,
          processBlockInRunning
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
      reportingStore <- ReportStore.store[F](rnodeStoreManager)
      blockReportAPI = {
        implicit val (ec, bs) = (engineCell, blockStore)
        BlockReportAPI[F](reportingRuntime, reportingStore)
      }
      apiServers = {
        implicit val (ec, bs) = (engineCell, blockStore)
        APIServers.build[F](
          evalRuntime,
          triggerProposeFOpt,
          proposerStateRefOpt,
          conf.apiServer.maxBlocksLimit,
          conf.devMode,
          if (conf.autopropose && conf.dev.deployerPrivateKey.isDefined) triggerProposeFOpt
          else none[ProposeFunction[F]],
          blockReportAPI
        )
      }
      reportingRoutes = {
        ReportingRoutes.service[F](blockReportAPI)
      }
      casperLoop = {
        implicit val br = blockRetriever
        for {
          _ <- BlockRetriever[F].requestAll(conf.casper.requestedBlocksTimeout)
          _ <- Time[F].sleep(conf.casper.casperLoopInterval)
        } yield ()
      }
      // Broadcast fork choice tips request if current fork choice is more then `forkChoiceStaleThreshold` minutes old.
      // For why - look at updateForkChoiceTipsIfStuck method description.
      updateForkChoiceLoop = {
        implicit val (ec, bs, cu) = (engineCell, blockStore, commUtil)
        for {
          _ <- Time[F].sleep(conf.casper.forkChoiceCheckIfStaleInterval)
          _ <- Running.updateForkChoiceTipsIfStuck(conf.casper.forkChoiceStaleThreshold)
        } yield ()
      }
      engineInit = engineCell.read >>= (_.init)
      runtimeCleanup = NodeRuntime.cleanup(
        rnodeStoreManager
      )
      webApi = {
        implicit val (ec, bs) = (engineCell, blockStore)
        new WebApiImpl[F](
          conf.apiServer.maxBlocksLimit,
          conf.devMode,
          if (conf.autopropose && conf.dev.deployerPrivateKey.isDefined) triggerProposeFOpt
          else none[ProposeFunction[F]]
        )
      }
      adminWebApi = {
        implicit val ec = (engineCell)
        new AdminWebApiImpl[F](
          triggerProposeFOpt,
          proposerStateRefOpt
        )
      }
    } yield (
      packetHandler,
      blockProcessingStream,
      apiServers,
      casperLoop,
      updateForkChoiceLoop,
      engineInit,
      casperLaunch,
      reportingRoutes,
      webApi,
      adminWebApi,
      proposerStream
    )
  }
}
