package coop.rchain.casper.genesis

import java.io.{File, PrintWriter}
import java.nio.file.Path

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import cats.{Applicative, Foldable, Monad}
import com.google.protobuf.ByteString
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil.{blockHeader, stringToByteString, unsignedBlockProto}
import coop.rchain.casper.util.{EventConverter, Sorting}
import coop.rchain.casper.util.rholang.{ProcessedDeployUtil, RuntimeManager}
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.{EventConverter, Sorting}
import coop.rchain.catscontrib._
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.shared.{Log, LogSource, Time}
import monix.execution.Scheduler

import scala.io.Source
import scala.util.{Failure, Success, Try}
import coop.rchain.casper.util.Sorting.byteArrayOrdering
import coop.rchain.rholang.interpreter.accounting
import monix.eval.Task

import scala.concurrent.duration.Duration

object Genesis {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def defaultBlessedTerms(
      timestamp: Long,
      posParams: ProofOfStakeParams,
      wallets: Seq[PreWallet],
      faucetCode: String => String
  ): List[DeployData] =
    List(
      StandardDeploys.listOps,
      StandardDeploys.either,
      StandardDeploys.nonNegativeNumber,
      StandardDeploys.makeMint,
      StandardDeploys.makePoS,
      StandardDeploys.basicWallet,
      StandardDeploys.basicWalletFaucet,
      StandardDeploys.walletCheck,
      StandardDeploys.systemInstances,
      StandardDeploys.lockbox,
      StandardDeploys.rev(wallets, faucetCode, posParams)
    )

  def withContracts[F[_]: Concurrent](
      initial: BlockMessage,
      posParams: ProofOfStakeParams,
      wallets: Seq[PreWallet],
      faucetCode: String => String,
      startHash: StateHash,
      runtimeManager: RuntimeManager[F],
      timestamp: Long
  ): F[BlockMessage] =
    withContracts(
      defaultBlessedTerms(timestamp, posParams, wallets, faucetCode),
      initial,
      startHash,
      runtimeManager
    )

  def withContracts[F[_]: Concurrent](
      blessedTerms: List[DeployData],
      initial: BlockMessage,
      startHash: StateHash,
      runtimeManager: RuntimeManager[F]
  ): F[BlockMessage] =
    runtimeManager
      .computeState(startHash, blessedTerms)
      .map {
        case (stateHash, processedDeploys) =>
          val stateWithContracts = for {
            bd <- initial.body
            ps <- bd.state
          } yield ps.withPreStateHash(runtimeManager.emptyStateHash).withPostStateHash(stateHash)
          val version   = initial.header.get.version
          val timestamp = initial.header.get.timestamp
          val blockDeploys =
            processedDeploys.filterNot(_.status.isFailed).map(ProcessedDeployUtil.fromInternal)
          val sortedDeploys = blockDeploys.map(d => d.copy(log = d.log.sortBy(_.toByteArray)))
          val body          = Body(state = stateWithContracts, deploys = sortedDeploys)
          val header        = blockHeader(body, List.empty[ByteString], version, timestamp)
          unsignedBlockProto(body, header, List.empty[Justification], initial.shardId)
      }

  def withoutContracts(
      bonds: Map[Array[Byte], Long],
      version: Long,
      timestamp: Long,
      shardId: String
  ): BlockMessage = {
    import Sorting.byteArrayOrdering
    //sort to have deterministic order (to get reproducible hash)
    val bondsProto = bonds.toIndexedSeq.sorted.map {
      case (pk, stake) =>
        val validator = ByteString.copyFrom(pk)
        Bond(validator, stake)
    }

    val state = RChainState()
      .withBlockNumber(0)
      .withBonds(bondsProto)
    val body = Body()
      .withState(state)
    val header = blockHeader(body, List.empty[ByteString], version, timestamp)

    unsignedBlockProto(body, header, List.empty[Justification], shardId)
  }

  //TODO: Decide on version number and shard identifier
  def fromInputFiles[F[_]: Concurrent: Sync: Log: Time](
      maybeBondsPath: Option[String],
      numValidators: Int,
      genesisPath: Path,
      maybeWalletsPath: Option[String],
      minimumBond: Long,
      maximumBond: Long,
      faucet: Boolean,
      runtimeManager: RuntimeManager[F],
      shardId: String,
      deployTimestamp: Option[Long]
  ): F[BlockMessage] =
    for {
      bondsFile <- toFile[F](maybeBondsPath, genesisPath.resolve("bonds.txt"))
      _ <- bondsFile.fold[F[Unit]](
            maybeBondsPath.fold(().pure[F])(
              path =>
                Log[F].warn(
                  s"Specified bonds file $path does not exist. Falling back on generating random validators."
                )
            )
          )(_ => ().pure[F])
      walletsFile <- toFile[F](maybeWalletsPath, genesisPath.resolve("wallets.txt"))
      wallets     <- getWallets[F](walletsFile, maybeWalletsPath)
      bonds       <- getBonds[F](bondsFile, numValidators, genesisPath)
      timestamp   <- deployTimestamp.fold(Time[F].currentMillis)(_.pure[F])
      initial     = withoutContracts(bonds = bonds, timestamp = 1L, version = 1L, shardId = shardId)
      validators  = bonds.map(bond => ProofOfStakeValidator(bond._1, bond._2)).toSeq
      faucetCode  = if (faucet) Faucet.basicWalletFaucet(_) else Faucet.noopFaucet
      withContr <- withContracts(
                    initial,
                    ProofOfStakeParams(minimumBond, maximumBond, validators),
                    wallets,
                    faucetCode,
                    runtimeManager.emptyStateHash,
                    runtimeManager,
                    timestamp
                  )
    } yield withContr

  def toFile[F[_]: Applicative: Log](
      maybePath: Option[String],
      defaultPath: Path
  ): F[Option[File]] =
    maybePath match {
      case Some(path) =>
        val f = new File(path)
        if (f.exists) f.some.pure[F]
        else {
          none[File].pure[F]
        }

      case None =>
        val default = defaultPath.toFile
        if (default.exists) {
          Log[F].info(
            s"Found default file ${default.getPath}."
          ) *> default.some.pure[F]
        } else none[File].pure[F]
    }

  def getWallets[F[_]: Monad: Sync: Log](
      walletsFile: Option[File],
      maybeWalletsPath: Option[String]
  ): F[Seq[PreWallet]] = {
    def walletFromFile(file: File): F[Seq[PreWallet]] =
      for {
        maybeLines <- Sync[F].delay { Try(Source.fromFile(file).getLines().toList) }
        wallets <- maybeLines match {
                    case Success(lines) =>
                      lines
                        .traverse(PreWallet.fromLine(_) match {
                          case Right(wallet) => wallet.some.pure[F]
                          case Left(errMsg) =>
                            Log[F]
                              .warn(s"Error in parsing wallets file: $errMsg")
                              .map(_ => none[PreWallet])
                        })
                        .map(_.flatten)
                    case Failure(ex) =>
                      Log[F]
                        .warn(
                          s"Failed to read ${file.getAbsolutePath()} for reason: ${ex.getMessage}"
                        )
                        .map(_ => Seq.empty[PreWallet])
                  }
      } yield wallets

    (walletsFile, maybeWalletsPath) match {
      case (Some(file), _) => walletFromFile(file)
      case (None, Some(path)) =>
        Log[F]
          .warn(s"Specified wallets file $path does not exist. No wallets will exist at genesis.")
          .map(_ => Seq.empty[PreWallet])
      case (None, None) =>
        Log[F]
          .warn(
            s"No wallets file specified and no default file found. No wallets will exist at genesis."
          )
          .map(_ => Seq.empty[PreWallet])
    }
  }

  def getBondedValidators[F[_]: Monad: Sync: Log](bondsFile: Option[String]): F[Set[ByteString]] =
    bondsFile match {
      case None => Set.empty[ByteString].pure[F]
      case Some(file) =>
        Sync[F]
          .delay {
            Try {
              Source
                .fromFile(file)
                .getLines()
                .map(line => {
                  val Array(pk, _) = line.trim.split(" ")
                  ByteString.copyFrom(Base16.unsafeDecode(pk))
                })
                .toSet
            }
          }
          .flatMap {
            case Failure(th) =>
              Log[F]
                .warn(s"Failed to parse bonded validators file $file for reason ${th.getMessage}")
                .map(_ => Set.empty)
            case Success(x) => x.pure[F]
          }
    }

  def getBonds[F[_]: Monad: Sync: Log](
      bondsFile: Option[File],
      numValidators: Int,
      genesisPath: Path
  ): F[Map[Array[Byte], Long]] =
    bondsFile match {
      case Some(file) =>
        Sync[F]
          .delay {
            Try {
              Source
                .fromFile(file)
                .getLines()
                .map(line => {
                  val Array(pk, stake) = line.trim.split(" ")
                  Base16.unsafeDecode(pk) -> (stake.toLong)
                })
                .toMap
            }
          }
          .flatMap {
            case Success(bonds) => bonds.pure[F]
            case Failure(_) =>
              Log[F].warn(
                s"Bonds file ${file.getPath} cannot be parsed. Falling back on generating random validators."
              ) *> newValidators[F](numValidators, genesisPath)
          }
      case None => newValidators[F](numValidators, genesisPath)
    }

  private def newValidators[F[_]: Monad: Sync: Log](
      numValidators: Int,
      genesisPath: Path
  ): F[Map[Array[Byte], Long]] = {
    val keys         = Vector.fill(numValidators)(Ed25519.newKeyPair)
    val (_, pubKeys) = keys.unzip
    val bonds        = pubKeys.zipWithIndex.toMap.mapValues(_.toLong + 1L)
    val genBondsFile = genesisPath.resolve(s"bonds.txt").toFile

    val skFiles =
      Sync[F].delay(genesisPath.toFile.mkdir()) >>
        Sync[F].delay {
          keys.foreach { //create files showing the secret key for each public key
            case (sec, pub) =>
              val sk      = Base16.encode(sec)
              val pk      = Base16.encode(pub)
              val skFile  = genesisPath.resolve(s"$pk.sk").toFile
              val printer = new PrintWriter(skFile)
              printer.println(sk)
              printer.close()
          }
        }

    //create bonds file for editing/future use
    for {
      _       <- skFiles
      printer <- Sync[F].delay { new PrintWriter(genBondsFile) }
      _ <- Foldable[List].foldM[F, (Array[Byte], Long), Unit](bonds.toList, ()) {
            case (_, (pub, stake)) =>
              val pk = Base16.encode(pub)
              Log[F].info(s"Created validator $pk with bond $stake") *>
                Sync[F].delay { printer.println(s"$pk $stake") }
          }
      _ <- Sync[F].delay { printer.close() }
    } yield bonds
  }

}
