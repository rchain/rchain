package coop.rchain.casper.engine

import cats.Parallel
import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.dag.state.BlockDagState
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper._
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.protocol._
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.comm._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.{BondsParser, VaultParser}
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.rspace.state.RSpaceStateManager
import coop.rchain.shared._

trait CasperLaunch[F[_]] {
  def launch(): F[Unit]
}

object CasperLaunch {

  // format: off
  def of[F[_]
    /* Execution */   : Concurrent: Parallel: ContextShift: Time
    /* Transport */   : TransportLayer: CommUtil: BlockRetriever: EventPublisher
    /* State */       : EnvVars: EngineCell: RPConfAsk: ConnectionsCell: LastApprovedBlock
    /* Rholang */     : RuntimeManager
    /* Storage */     : BlockStore: BlockDagStorage: DeployStorage: RSpaceStateManager
    /* Diagnostics */ : Log: EventLog: Metrics: Span] // format: on
  (
      blockDagStateRef: Ref[F, BlockDagState],
      proposeFOpt: Option[ProposeFunction[F]],
      conf: CasperConf,
      trimState: Boolean,
      disableStateExporter: Boolean,
      processBlockInRunning: BlockMessage => F[Unit]
  ): CasperLaunch[F] =
    new CasperLaunch[F] {
      val casperShardConf = CasperShardConf(
        conf.faultToleranceThreshold,
        conf.shardName,
        conf.parentShardId,
        conf.finalizationRate,
        conf.maxNumberOfParents,
        conf.maxParentDepth.getOrElse(Int.MaxValue),
        conf.synchronyConstraintThreshold.toFloat,
        conf.heightConstraintThreshold,
        50,
        1,
        1,
        conf.genesisBlockData.bondMinimum,
        conf.genesisBlockData.bondMaximum,
        conf.genesisBlockData.epochLength,
        conf.genesisBlockData.quarantineLength,
        conf.minPhloPrice
      )
      def launch(): F[Unit] =
        BlockStore[F].getApprovedBlock map {
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

        for {
          validatorId <- ValidatorIdentity.fromPrivateKeyWithLogging[F](conf.validatorPrivateKey)
          casper <- MultiParentCasper
                     .hashSetCasper[F](
                       validatorId,
                       casperShardConf.shardName,
                       casperShardConf.faultToleranceThreshold,
                       casperShardConf.minPhloPrice
                     )
          init = for {
            _ <- askPeersForForkChoiceTips
            // try to propose (async way) if proposer is defined
            _ <- proposeFOpt.traverse(p => p(true))
          } yield ()
          _ <- Engine
                .transitionToRunning[F](
                  casper,
                  blockDagStateRef,
                  approvedBlock,
                  validatorId,
                  init,
                  disableStateExporter,
                  processBlockInRunning
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

          validatorId <- ValidatorIdentity.fromPrivateKeyWithLogging[F](conf.validatorPrivateKey)
          vaults      <- VaultParser.parse(conf.genesisBlockData.walletsFile)
          bap <- BlockApproverProtocol.of(
                  validatorId.get,
                  timestamp,
                  vaults,
                  bonds,
                  conf.genesisBlockData.bondMinimum,
                  conf.genesisBlockData.bondMaximum,
                  conf.genesisBlockData.epochLength,
                  conf.genesisBlockData.quarantineLength,
                  conf.genesisBlockData.numberOfActiveValidators,
                  conf.genesisCeremony.requiredSignatures
                )(Sync[F])
          _ <- EngineCell[F].set(
                new GenesisValidator(
                  blockDagStateRef,
                  casperShardConf,
                  validatorId.get,
                  bap,
                  processBlockInRunning
                )
              )
        } yield ()

      private def initBootstrap(disableStateExporter: Boolean): F[Unit] =
        for {
          validatorId <- ValidatorIdentity.fromPrivateKeyWithLogging[F](conf.validatorPrivateKey)
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
                    ByteString.copyFrom(validatorId.get.publicKey.bytes)
                  )
          // TODO track fiber
          _ <- Concurrent[F].start(
                GenesisCeremonyMaster
                  .waitingForApprovedBlockLoop[F](
                    blockDagStateRef,
                    casperShardConf,
                    validatorId,
                    disableStateExporter,
                    processBlockInRunning
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
                blockDagStateRef,
                casperShardConf,
                validatorId,
                // TODO peer should be able to request approved blocks on different heights
                // from genesis to the most recent one (default)
                CommUtil[F].requestApprovedBlock(trimState),
                trimState,
                disableStateExporter,
                processBlockInRunning
              )
        } yield ()

    }
}
