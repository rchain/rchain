package coop.rchain.casper.engine

import java.nio.file.Paths

import cats.Monad
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
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

object CasperLaunch {

  class CasperInit[F[_]](
      val conf: CasperConf
  )
  def apply[F[_]: LastApprovedBlock: Metrics: Span: BlockStore: ConnectionsCell: TransportLayer: RPConfAsk: SafetyOracle: LastFinalizedBlockCalculator: Sync: Concurrent: Time: Log: EventLog: BlockDagStorage: EngineCell: EnvVars: RaiseIOError: RuntimeManager: Running.RequestedBlocks](
      init: CasperInit[F]
  ): F[Unit] =
    BlockStore[F].getApprovedBlock map {
      case Some(approvedBlock) =>
        val msg    = "Found ApprovedBlock in storage, reconnecting to existing network"
        val action = connectToExistingNetwork[F](approvedBlock, init)
        (msg, action)
      case None if (init.conf.approveGenesis) =>
        val msg    = "ApprovedBlock not found in storage, taking part in ceremony as genesis validator"
        val action = connectAsGenesisValidator[F](init)
        (msg, action)
      case None if (init.conf.createGenesis) =>
        val msg =
          "ApprovedBlock not found in storage, taking part in ceremony as ceremony master"
        val action = initBootstrap[F](init)
        (msg, action)
      case None =>
        val msg    = "ApprovedBlock not found in storage, connecting to existing network"
        val action = connectAndQueryApprovedBlock[F](init)
        (msg, action)
    } >>= {
      case (msg, action) => Log[F].info(msg) >> action
    }

  def connectToExistingNetwork[F[_]: LastApprovedBlock: Metrics: Span: BlockStore: ConnectionsCell: TransportLayer: RPConfAsk: SafetyOracle: LastFinalizedBlockCalculator: Concurrent: Time: Log: EventLog: BlockDagStorage: EngineCell: EnvVars: RuntimeManager: Running.RequestedBlocks](
      approvedBlock: ApprovedBlock,
      init: CasperInit[F]
  ): F[Unit] =
    for {
      validatorId <- ValidatorIdentity.fromConfig[F](init.conf)
      genesis     = approvedBlock.candidate.flatMap(_.block).get
      casper <- MultiParentCasper
                 .hashSetCasper[F](validatorId, genesis, init.conf.shardId)
      _ <- Engine
            .transitionToRunning[F](casper, approvedBlock, CommUtil.sendForkChoiceTipRequest[F])
    } yield ()

  def connectAsGenesisValidator[F[_]: Monad: Sync: Metrics: Span: LastApprovedBlock: Time: Concurrent: Log: EventLog: RPConfAsk: BlockStore: ConnectionsCell: TransportLayer: SafetyOracle: LastFinalizedBlockCalculator: BlockDagStorage: EngineCell: EnvVars: RaiseIOError: RuntimeManager: Running.RequestedBlocks](
      init: CasperInit[F]
  ): F[Unit] =
    for {
      timestamp <- init.conf.deployTimestamp.fold(Time[F].currentMillis)(_.pure[F])
      bonds <- BondsParser.parse[F](
                init.conf.bondsFile,
                init.conf.genesisPath.resolve("bonds.txt"),
                init.conf.numValidators,
                init.conf.genesisPath
              )

      validatorId <- ValidatorIdentity.fromConfig[F](init.conf)
      vaults <- VaultParser.parse(
                 init.conf.walletsFile
                   .map(Paths.get(_))
                   .getOrElse(init.conf.genesisPath.resolve("wallets.txt"))
               )
      bap <- BlockApproverProtocol.of(
              validatorId.get,
              timestamp,
              vaults,
              bonds,
              init.conf.minimumBond,
              init.conf.maximumBond,
              init.conf.requiredSigs
            )(Sync[F])
      _ <- EngineCell[F].set(
            new GenesisValidator(validatorId.get, init.conf.shardId, bap)
          )
    } yield ()

  def initBootstrap[F[_]: Monad: Sync: LastApprovedBlock: Time: Log: EventLog: RPConfAsk: BlockStore: ConnectionsCell: TransportLayer: Concurrent: Metrics: Span: SafetyOracle: LastFinalizedBlockCalculator: BlockDagStorage: EngineCell: EnvVars: RaiseIOError: RuntimeManager: Running.RequestedBlocks](
      init: CasperInit[F]
  ): F[Unit] =
    for {
      validatorId <- ValidatorIdentity.fromConfig[F](init.conf)
      abp <- ApproveBlockProtocol
              .of[F](
                init.conf.bondsFile,
                init.conf.numValidators,
                init.conf.genesisPath,
                init.conf.walletsFile,
                init.conf.minimumBond,
                init.conf.maximumBond,
                init.conf.shardId,
                init.conf.deployTimestamp,
                init.conf.requiredSigs,
                init.conf.approveGenesisDuration,
                init.conf.approveGenesisInterval
              )
      // TODO track fiber
      _ <- Concurrent[F].start(
            GenesisCeremonyMaster
              .approveBlockInterval[F](
                init.conf.approveGenesisInterval,
                init.conf.shardId,
                validatorId
              )
          )
      _ <- EngineCell[F].set(new GenesisCeremonyMaster[F](abp))
    } yield ()

  def connectAndQueryApprovedBlock[F[_]: Monad: Sync: LastApprovedBlock: Time: Log: EventLog: RPConfAsk: BlockStore: ConnectionsCell: TransportLayer: Metrics: Span: Concurrent: SafetyOracle: LastFinalizedBlockCalculator: BlockDagStorage: EnvVars: EngineCell: RuntimeManager: Running.RequestedBlocks](
      init: CasperInit[F]
  ): F[Unit] =
    for {
      validatorId <- ValidatorIdentity.fromConfig[F](init.conf)
      _ <- Engine.transitionToInitializing(
            init.conf.shardId,
            validatorId,
            CommUtil.requestApprovedBlock[F]
          )
    } yield ()
}
