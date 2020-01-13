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
        case None if (conf.approveGenesis) =>
          val msg =
            "ApprovedBlock not found in storage, taking part in ceremony as genesis validator"
          val action = connectAsGenesisValidator()
          (msg, action)
        case None if (conf.createGenesis) =>
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
        validatorId <- ValidatorIdentity.fromConfig[F](conf)
        genesis     = approvedBlock.candidate.block
        casper <- MultiParentCasper
                   .hashSetCasper[F](validatorId, genesis, conf.shardId, conf.finalizationRate)
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
        timestamp <- conf.deployTimestamp.fold(Time[F].currentMillis)(_.pure[F])
        bonds <- BondsParser.parse[F](
                  conf.bondsFile,
                  conf.genesisPath.resolve("bonds.txt"),
                  conf.numValidators,
                  conf.genesisPath
                )

        validatorId <- ValidatorIdentity.fromConfig[F](conf)
        vaults <- VaultParser.parse(
                   conf.walletsFile
                     .map(Paths.get(_))
                     .getOrElse(conf.genesisPath.resolve("wallets.txt"))
                 )
        bap <- BlockApproverProtocol.of(
                validatorId.get,
                timestamp,
                vaults,
                bonds,
                conf.minimumBond,
                conf.maximumBond,
                conf.epochLength,
                conf.quarantineLength,
                conf.numberOfActiveValidators,
                conf.requiredSigs
              )(Sync[F])
        _ <- EngineCell[F].set(
              new GenesisValidator(validatorId.get, conf.shardId, conf.finalizationRate, bap)
            )
      } yield ()

    private def initBootstrap(): F[Unit] =
      for {
        validatorId <- ValidatorIdentity.fromConfig[F](conf)
        abp <- ApproveBlockProtocol
                .of[F](
                  conf.bondsFile,
                  conf.numValidators,
                  conf.genesisPath,
                  conf.walletsFile,
                  conf.minimumBond,
                  conf.maximumBond,
                  conf.epochLength,
                  conf.quarantineLength,
                  conf.numberOfActiveValidators,
                  conf.shardId,
                  conf.deployTimestamp,
                  conf.requiredSigs,
                  conf.approveGenesisDuration,
                  conf.approveGenesisInterval
                )
        // TODO track fiber
        _ <- Concurrent[F].start(
              GenesisCeremonyMaster
                .approveBlockInterval[F](
                  conf.approveGenesisInterval,
                  conf.shardId,
                  conf.finalizationRate,
                  validatorId
                )
            )
        _ <- EngineCell[F].set(new GenesisCeremonyMaster[F](abp))
      } yield ()

    private def connectAndQueryApprovedBlock(): F[Unit] =
      for {
        validatorId <- ValidatorIdentity.fromConfig[F](conf)
        _ <- Engine.transitionToInitializing(
              conf.shardId,
              conf.finalizationRate,
              validatorId,
              CommUtil[F].requestApprovedBlock
            )
      } yield ()
  }
}
