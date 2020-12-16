package coop.rchain.casper.engine

import java.nio.file.Paths

import cats.Parallel
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.casperbuffer.CasperBufferStorage
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.blockstorage.finality.LastFinalizedStorage
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
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
    /* Execution */   : Concurrent: Parallel: Time: RaiseIOError
    /* Transport */   : TransportLayer: CommUtil: BlockRetriever: EventPublisher
    /* State */       : EnvVars: EngineCell: RPConfAsk: ConnectionsCell: LastApprovedBlock
    /* Rholang */     : RuntimeManager
    /* Casper */      : Estimator: SafetyOracle: LastFinalizedBlockCalculator: LastFinalizedHeightConstraintChecker: SynchronyConstraintChecker
    /* Storage */     : BlockStore: BlockDagStorage: LastFinalizedStorage: DeployStorage: CasperBufferStorage: RSpaceStateManager
    /* Diagnostics */ : Log: EventLog: Metrics: Span] // format: on
  (
      conf: CasperConf,
      trimState: Boolean,
      disableStateExporter: Boolean
  ): CasperLaunch[F] =
    new CasperLaunch[F] {
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
        def sendBufferPendantsToCasper(casper: Casper[F]) =
          for {
            pendants <- CasperBufferStorage[F].getPendants
            // pendantsReceived are either
            // 1. blocks that were received while catching up but not end up in casper buffer, e.g. node were restarted
            // or
            // 2. blocks which dependencies are in DAG, so they can be added to DAG
            // In both scenarios the way to proceed is to send them to Casper
            pendantsStored <- pendants.toList.filterA(BlockStore[F].contains)
            _ <- Log[F].info(
                  s"Checking pendant hashes: ${pendantsStored.size} items in CasperBuffer."
                )
            _ <- pendantsStored
                // we just need to send blocks to Casper. Nothing to do with results of block processing here,
                // so ignoring them
                  .traverse_(
                    hash =>
                      for {
                        block <- BlockStore[F].get(hash).map(_.get)
                        _ <- Log[F].info(
                              s"Pendant ${PrettyPrinter.buildString(block, short = true)} " +
                                s"is available in BlockStore, sending to Casper."
                            )
                        dc <- casper.dagContains(hash)
                        _ <- Log[F]
                              .error(
                                s"Pendant ${PrettyPrinter.buildString(block, short = true)} " +
                                  s"is available in DAG, database is supposedly in inconsistent state."
                              )
                              .whenA(dc)
                        _ <- BlockRetriever[F].ackReceive(hash)
                        _ <- casper.addBlockFromStore(hash, allowAddFromBuffer = true)
                      } yield ()
                  )
          } yield ()

        for {
          validatorId <- ValidatorIdentity.fromPrivateKeyWithLogging[F](conf.validatorPrivateKey)
          ab          = approvedBlock.candidate.block
          casper <- MultiParentCasper
                     .hashSetCasper[F](
                       validatorId,
                       ab,
                       conf.shardName,
                       conf.finalizationRate
                     )
          init = for {
            _ <- askPeersForForkChoiceTips
            _ <- sendBufferPendantsToCasper(casper)
          } yield ()
          _ <- Engine
                .transitionToRunning[F](
                  casper,
                  approvedBlock,
                  validatorId,
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
                    conf.genesisBlockData.genesisDataDir.resolve("bonds.txt"),
                    conf.genesisCeremony.autogenShardSize,
                    conf.genesisBlockData.genesisDataDir
                  )

          validatorId <- ValidatorIdentity.fromPrivateKeyWithLogging[F](conf.validatorPrivateKey)
          vaults <- VaultParser.parse(
                     conf.genesisBlockData.walletsFile
                       .map(Paths.get(_))
                       .getOrElse(conf.genesisBlockData.genesisDataDir.resolve("wallets.txt"))
                   )
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
                new GenesisValidator(validatorId.get, conf.shardName, conf.finalizationRate, bap)
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
                    conf.genesisCeremony.approveInterval
                  )
          // TODO track fiber
          _ <- Concurrent[F].start(
                GenesisCeremonyMaster
                  .waitingForApprovedBlockLoop[F](
                    conf.shardName,
                    conf.finalizationRate,
                    validatorId,
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
                conf.shardName,
                conf.finalizationRate,
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
