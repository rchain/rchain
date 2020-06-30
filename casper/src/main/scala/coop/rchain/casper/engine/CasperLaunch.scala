package coop.rchain.casper.engine

import java.nio.file.Paths

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import coop.rchain.blockstorage.BlockStore
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
import coop.rchain.shared._

trait CasperLaunch[F[_]] {
  def launch(): F[Unit]
}

object CasperLaunch {

  def of[F[_]: LastApprovedBlock: Metrics: Span: BlockStore: CommUtil: ConnectionsCell: TransportLayer: RPConfAsk: SafetyOracle: LastFinalizedBlockCalculator: Concurrent: Time: Log: EventLog: BlockDagStorage: LastFinalizedStorage: EngineCell: EnvVars: RaiseIOError: RuntimeManager: Running.RequestedBlocks: EventPublisher: SynchronyConstraintChecker: LastFinalizedHeightConstraintChecker: Estimator: DeployStorage](
      conf: CasperConf
  ): CasperLaunch[F] = new CasperLaunch[F] {

    def launch(): F[Unit] =
      BlockStore[F].getApprovedBlock map {
        case Some(approvedBlock) =>
          val msg    = "Found ApprovedBlock in storage, reconnecting to existing network"
          val action = connectToExistingNetwork(approvedBlock)
          (msg, action)
        case None if (conf.genesisCeremony.genesisValidatorMode) =>
          val msg =
            "ApprovedBlock not found in storage, taking part in ceremony as genesis validator"
          val action = connectAsGenesisValidator()
          (msg, action)
        case None if (conf.genesisCeremony.ceremonyMasterMode) =>
          val msg =
            "ApprovedBlock not found in storage, taking part in ceremony as ceremony master"
          val action = initBootstrap()
          (msg, action)
        case None =>
          val msg    = "ApprovedBlock not found in storage, connecting to existing network"
          val action = connectAndQueryApprovedBlock()
          (msg, action)
      } >>= {
        case (msg, action) => Log[F].info(msg) >> action
      }

    private def connectToExistingNetwork(approvedBlock: ApprovedBlock): F[Unit] =
      for {
        validatorId <- ValidatorIdentity.fromPrivateKeyWithLogging[F](conf.validatorPrivateKey)
        genesis     = approvedBlock.candidate.block
        casper <- MultiParentCasper
                   .hashSetCasper[F](
                     validatorId,
                     genesis,
                     conf.shardName,
                     conf.finalizationRate,
                     skipValidateGenesis = true
                   )
        _ <- Engine
              .transitionToRunning[F](
                casper,
                approvedBlock,
                validatorId,
                CommUtil[F].sendForkChoiceTipRequest
              )
      } yield ()

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

    private def initBootstrap(): F[Unit] =
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
                  validatorId
                )
            )
        _ <- EngineCell[F].set(new GenesisCeremonyMaster[F](abp))
      } yield ()

    private def connectAndQueryApprovedBlock(): F[Unit] =
      for {
        validatorId <- ValidatorIdentity.fromPrivateKeyWithLogging[F](conf.validatorPrivateKey)
        _ <- Engine.transitionToInitializing(
              conf.shardName,
              conf.finalizationRate,
              validatorId,
              CommUtil[F].requestApprovedBlock
            )
      } yield ()
  }
}
