package coop.rchain.casper.engine

import cats.Parallel
import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, ContextShift, Sync, Timer}
import cats.syntax.all._
import coop.rchain.blockstorage.approvedStore.ApprovedStore
import coop.rchain.blockstorage.blockStore.BlockStore
import coop.rchain.blockstorage.casperbuffer.CasperBufferStorage
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper._
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.protocol._
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.{BondsParser, VaultParser}
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.rspace.state.RSpaceStateManager
import coop.rchain.shared._
import coop.rchain.shared.syntax._
import fs2.concurrent.Queue

trait CasperLaunch[F[_]] {
  def launch(): F[Unit]
}

object CasperLaunch {

  // format: off
  def of[F[_]
    /* Execution */   : Concurrent: Parallel: ContextShift: Time: Timer
    /* Transport */   : TransportLayer: CommUtil: BlockRetriever: EventPublisher
    /* State */       : EnvVars: EngineCell: RPConfAsk: ConnectionsCell: LastApprovedBlock
    /* Rholang */     : RuntimeManager
    /* Casper */      : LastFinalizedHeightConstraintChecker: SynchronyConstraintChecker
    /* Storage */     : BlockStore: ApprovedStore: BlockDagStorage: DeployStorage: CasperBufferStorage: RSpaceStateManager
    /* Diagnostics */ : Log: EventLog: Metrics: Span] // format: on
  (
      blockProcessingQueue: Queue[F, BlockMessage],
      blocksInProcessing: Ref[F, Set[BlockHash]],
      proposeFOpt: Option[ProposeFunction[F]],
      conf: CasperConf,
      trimState: Boolean,
      disableStateExporter: Boolean,
      validatorIdentityOpt: Option[ValidatorIdentity],
      casperShardConf: CasperShardConf
  ): CasperLaunch[F] =
    new CasperLaunch[F] {
      def launch(): F[Unit] =
        ApprovedStore[F].getApprovedBlock map {
          case Some(approvedBlock) =>
            val msg = "Approved block found, reconnecting to existing network"
            val action =
              connectToExistingNetwork(approvedBlock, disableStateExporter)
            (msg, action)
          case None if (conf.genesisCeremony.genesisValidatorMode) =>
            val msg =
              "Approved block not found, taking part in ceremony as genesis validator"
            val action = connectAsGenesisValidator()
            (msg, action)
          case None if (conf.genesisCeremony.ceremonyMasterMode) =>
            val msg =
              "Approved block not found, taking part in ceremony as ceremony master"
            val action = initBootstrap(disableStateExporter)
            (msg, action)
          case None =>
            val msg = "Approved block not found, connecting to existing network"
            val action = connectAndQueryApprovedBlock(
              trimState,
              disableStateExporter
            )
            (msg, action)
        } >>= {
          case (msg, action) => Log[F].info(msg) >> action
        }

      private def connectToExistingNetwork(
          approvedBlock: ApprovedBlock,
          disableStateExporter: Boolean
      ): F[Unit] = {
        def askPeersForForkChoiceTips = CommUtil[F].sendForkChoiceTipRequest
        def sendBufferPendantsToCasper =
          for {
            pendants <- CasperBufferStorage[F].getPendants
            // pendantsReceived are either
            // 1. blocks that were received while catching up but not end up in casper buffer, e.g. node were restarted
            // or
            // 2. blocks which dependencies are in DAG, so they can be added to DAG
            // In both scenarios the way to proceed is to send them to Casper
            pendantsStored <- pendants.toList.filterA(BlockStore[F].contains(_))
            _ <- Log[F].info(
                  s"Checking pendant hashes: ${pendantsStored.size} items in CasperBuffer."
                )
            _ <- pendantsStored
                // we just need to send blocks to Casper. Nothing to do with results of block processing here,
                // so ignoring them
                  .traverse_(
                    hash =>
                      for {
                        block <- BlockStore[F].get1(hash).map(_.get)
                        _ <- Log[F].info(
                              s"Pendant ${PrettyPrinter.buildString(block, short = true)} " +
                                s"is available in BlockStore, sending to Casper."
                            )
                        dag <- BlockDagStorage[F].getRepresentation
                        dc  = dag.contains(hash)
                        _ <- Log[F]
                              .error(
                                s"Pendant ${PrettyPrinter.buildString(block, short = true)} " +
                                  s"is available in DAG, database is supposedly in inconsistent state."
                              )
                              .whenA(dc)
                        _ <- BlockRetriever[F].ackReceive(hash)
                        _ <- blockProcessingQueue.enqueue1(block)
                      } yield ()
                  )
          } yield ()

        val init = for {
          _ <- askPeersForForkChoiceTips
          _ <- sendBufferPendantsToCasper
          // try to propose (async way) if proposer is defined
          _ <- proposeFOpt.traverse(_(true))
        } yield ()
        for {
          _ <- Engine
                .transitionToRunning[F](
                  blockProcessingQueue,
                  blocksInProcessing,
                  approvedBlock,
                  validatorIdentityOpt,
                  init,
                  disableStateExporter
                )
        } yield ()
      }

      private def connectAsGenesisValidator(): F[Unit] =
        for {
          timestamp <- conf.genesisBlockData.deployTimestamp.fold(Time[F].currentMillis)(_.pure[F])
          bonds <- BondsParser.parse[F](
                    conf.genesisBlockData.bondsFile,
                    conf.genesisCeremony.autogenShardSize
                  )

          vaults <- VaultParser.parse(conf.genesisBlockData.walletsFile)
          validatorId <- validatorIdentityOpt.liftTo(
                          new Exception(s"Validator private key is missing.")
                        )
          bap <- BlockApproverProtocol.of(
                  validatorId,
                  timestamp,
                  vaults,
                  bonds,
                  conf.genesisBlockData.bondMinimum,
                  conf.genesisBlockData.bondMaximum,
                  conf.genesisBlockData.epochLength,
                  conf.genesisBlockData.quarantineLength,
                  conf.genesisBlockData.numberOfActiveValidators,
                  conf.genesisCeremony.requiredSignatures,
                  conf.genesisBlockData.posMultiSigPublicKeys,
                  conf.genesisBlockData.posMultiSigQuorum
                )(Sync[F])

          _ <- EngineCell[F].set(
                new GenesisValidator(
                  blockProcessingQueue,
                  blocksInProcessing,
                  casperShardConf,
                  validatorId,
                  bap
                )
              )
        } yield ()

      private def initBootstrap(disableStateExporter: Boolean): F[Unit] =
        for {
          abp <- ApproveBlockProtocol
                  .of[F](
                    conf.genesisBlockData.bondsFile,
                    conf.genesisCeremony.autogenShardSize,
                    conf.genesisBlockData.genesisDataDir,
                    conf.genesisBlockData.walletsFile,
                    conf.genesisBlockData.bondMinimum,
                    conf.genesisBlockData.bondMaximum,
                    conf.genesisBlockData.epochLength,
                    conf.genesisBlockData.quarantineLength,
                    conf.genesisBlockData.numberOfActiveValidators,
                    conf.shardName,
                    conf.genesisBlockData.deployTimestamp,
                    conf.genesisCeremony.requiredSignatures,
                    conf.genesisCeremony.approveDuration,
                    conf.genesisCeremony.approveInterval,
                    conf.genesisBlockData.genesisBlockNumber,
                    conf.genesisBlockData.posMultiSigPublicKeys,
                    conf.genesisBlockData.posMultiSigQuorum
                  )
          // TODO track fiber
          _ <- Concurrent[F].start(
                GenesisCeremonyMaster
                  .waitingForApprovedBlockLoop[F](
                    blockProcessingQueue,
                    blocksInProcessing,
                    casperShardConf,
                    validatorIdentityOpt,
                    disableStateExporter
                  )
              )
          _ <- EngineCell[F].set(new GenesisCeremonyMaster[F](abp))
        } yield ()

      private def connectAndQueryApprovedBlock(
          trimState: Boolean,
          disableStateExporter: Boolean
      ): F[Unit] =
        for {
          validatorId <- ValidatorIdentity.fromPrivateKeyWithLogging[F](conf.validatorPrivateKey)
          _ <- Engine.transitionToInitializing(
                blockProcessingQueue,
                blocksInProcessing,
                casperShardConf,
                validatorId,
                // TODO peer should be able to request approved blocks on different heights
                // from genesis to the most recent one (default)
                CommUtil[F].requestApprovedBlock(trimState),
                trimState,
                disableStateExporter
              )
        } yield ()

    }
}
