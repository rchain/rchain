package coop.rchain.casper.engine

import cats.data.EitherT
import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import cats.{Applicative, Monad}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.{BlockDagStorage, BlockStore}
import coop.rchain.casper.Estimator.Validator
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.casper._
import coop.rchain.casper.util.comm._
import coop.rchain.casper.engine._, EngineCell._
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.MonadTrans
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm.discovery.NodeDiscovery
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.{Blob, TransportLayer}
import coop.rchain.comm.{transport, PeerNode}
import coop.rchain.metrics.Metrics
import coop.rchain.shared.{Log, LogSource, Time}
import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.duration.FiniteDuration
import scala.util.Try

object CasperLaunch {

  class CasperInit[F[_]](
      val conf: CasperConf,
      val runtimeManager: RuntimeManager[F]
  )
  def apply[F[_]: LastApprovedBlock: Metrics: BlockStore: ConnectionsCell: NodeDiscovery: TransportLayer: ErrorHandler: RPConfAsk: SafetyOracle: Sync: Concurrent: Time: Log: MultiParentCasperRef: BlockDagStorage: EngineCell](
      init: CasperInit[F],
      toTask: F[_] => Task[_]
  )(implicit scheduler: Scheduler): F[Unit] =
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
        val action = initBootstrap[F](init, toTask)
        (msg, action)
      case None =>
        val msg    = "ApprovedBlock not found in storage, connecting to existing network"
        val action = connectAndQueryApprovedBlock[F](init)
        (msg, action)
    } >>= {
      case (msg, action) => Log[F].info(msg) >> action
    }

  def connectToExistingNetwork[F[_]: LastApprovedBlock: Metrics: BlockStore: ConnectionsCell: NodeDiscovery: TransportLayer: ErrorHandler: RPConfAsk: SafetyOracle: Concurrent: Time: Log: MultiParentCasperRef: BlockDagStorage: EngineCell](
      approvedBlock: ApprovedBlock,
      init: CasperInit[F]
  ): F[Unit] =
    for {
      validatorId <- ValidatorIdentity.fromConfig[F](init.conf)
      genesis     = approvedBlock.candidate.flatMap(_.block).get
      casper <- MultiParentCasper.hashSetCasper[F](
                 init.runtimeManager,
                 validatorId,
                 genesis,
                 init.conf.shardId
               )
      _ <- Engine.transitionToRunning[F](casper, approvedBlock)
    } yield ()

  def connectAsGenesisValidator[F[_]: Monad: Sync: Metrics: LastApprovedBlock: ErrorHandler: Time: Concurrent: MultiParentCasperRef: Log: RPConfAsk: BlockStore: ConnectionsCell: TransportLayer: SafetyOracle: BlockDagStorage: EngineCell](
      init: CasperInit[F]
  ): F[Unit] =
    for {
      walletsFile <- Genesis
                      .toFile[F](
                        init.conf.walletsFile,
                        init.conf.genesisPath.resolve("wallets.txt")
                      )
      wallets   <- Genesis.getWallets[F](walletsFile, init.conf.walletsFile)
      timestamp <- init.conf.deployTimestamp.fold(Time[F].currentMillis)(_.pure[F])
      bondsFile <- Genesis
                    .toFile[F](init.conf.bondsFile, init.conf.genesisPath.resolve("bonds.txt"))
      bonds <- Genesis
                .getBonds[F](bondsFile, init.conf.numValidators, init.conf.genesisPath)
      validatorId <- ValidatorIdentity.fromConfig[F](init.conf)
      bap = new BlockApproverProtocol(
        validatorId.get,
        timestamp,
        bonds,
        wallets,
        init.conf.minimumBond,
        init.conf.maximumBond,
        init.conf.hasFaucet,
        init.conf.requiredSigs
      )
      _ <- EngineCell[F].set(
            new GenesisValidator(
              init.runtimeManager,
              validatorId.get,
              init.conf.shardId,
              bap
            )
          )
    } yield ()

  def initBootstrap[F[_]: Monad: Sync: LastApprovedBlock: ErrorHandler: Time: MultiParentCasperRef: Log: RPConfAsk: BlockStore: ConnectionsCell: TransportLayer: Concurrent: Metrics: SafetyOracle: BlockDagStorage: EngineCell](
      init: CasperInit[F],
      toTask: F[_] => Task[_]
  )(implicit scheduler: Scheduler): F[Unit] =
    for {
      genesis <- Genesis.fromInputFiles[F](
                  init.conf.bondsFile,
                  init.conf.numValidators,
                  init.conf.genesisPath,
                  init.conf.walletsFile,
                  init.conf.minimumBond,
                  init.conf.maximumBond,
                  init.conf.hasFaucet,
                  init.runtimeManager,
                  init.conf.shardId,
                  init.conf.deployTimestamp
                )
      validatorId <- ValidatorIdentity.fromConfig[F](init.conf)
      bondedValidators = genesis.body
        .flatMap(_.state.map(_.bonds.map(_.validator).toSet))
        .getOrElse(Set.empty)
      abp <- ApproveBlockProtocol
              .of[F](
                genesis,
                bondedValidators,
                init.conf.requiredSigs,
                init.conf.approveGenesisDuration,
                init.conf.approveGenesisInterval
              )
      // TODO OMG Fix, use Concurrent+!11
      _ <- Sync[F].delay {
            val _ = toTask(
              GenesisCeremonyMaster
                .approveBlockInterval(
                  init.conf.approveGenesisInterval,
                  init.conf.shardId,
                  init.runtimeManager,
                  validatorId
                )
            ).forkAndForget.runToFuture
            ().pure[F]
          }
      _ <- EngineCell[F].set(new GenesisCeremonyMaster[F](abp))
    } yield ()

  def connectAndQueryApprovedBlock[F[_]: Monad: Sync: LastApprovedBlock: ErrorHandler: Time: MultiParentCasperRef: Log: RPConfAsk: BlockStore: ConnectionsCell: TransportLayer: Metrics: Concurrent: SafetyOracle: BlockDagStorage: EngineCell](
      init: CasperInit[F]
  ): F[Unit] =
    for {
      validators  <- CasperConf.parseValidatorsFile[F](init.conf.knownValidatorsFile)
      validatorId <- ValidatorIdentity.fromConfig[F](init.conf)
      _ <- Engine.tranistionToInitializing(
            init.runtimeManager,
            init.conf.shardId,
            validatorId,
            validators,
            CommUtil.requestApprovedBlock[F]
          )
    } yield ()
}
