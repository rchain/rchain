package coop.rchain.casper.genesis

import cats.{Applicative, Foldable, Monad}
import cats.implicits._

import com.google.protobuf.ByteString

import coop.rchain.catscontrib._
import coop.rchain.casper.genesis.contracts.{Rev, Wallet}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil.{blockHeader, termDeploy, unsignedBlockProto}
import coop.rchain.casper.util.{EventConverter, Sorting}
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.rholang.collection.LinkedList
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.math.NonNegativeNumber
import coop.rchain.rholang.mint.MakeMint
import coop.rchain.rholang.wallet.BasicWallet
import coop.rchain.shared.{Log, LogSource, Time}

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Path}

import monix.execution.Scheduler

import scala.concurrent.SyncVar
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Genesis {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def withContracts(initial: BlockMessage,
                    wallets: Seq[Wallet],
                    startHash: StateHash,
                    runtimeManager: RuntimeManager)(implicit scheduler: Scheduler): BlockMessage = {

    val blessedTerms = List(
      LinkedList.term,
      NonNegativeNumber.term,
      MakeMint.term,
      BasicWallet.term,
      (new Rev(wallets)).term
    ).map(termDeploy)

    val Right(checkpoint) = runtimeManager.computeState(startHash, blessedTerms)
    val stateHash         = ByteString.copyFrom(checkpoint.root.bytes.toArray)
    val reductionLog      = checkpoint.log.map(EventConverter.toCasperEvent)

    val stateWithContracts = for {
      bd <- initial.body
      ps <- bd.postState
    } yield ps.withTuplespace(stateHash)
    val version   = initial.header.get.version
    val timestamp = initial.header.get.timestamp

    val body =
      Body(postState = stateWithContracts, newCode = blessedTerms, commReductions = reductionLog)
    val header = blockHeader(body, List.empty[ByteString], version, timestamp)

    unsignedBlockProto(body, header, List.empty[Justification])
  }

  def withoutContracts(bonds: Map[Array[Byte], Int],
                       version: Long,
                       timestamp: Long): BlockMessage = {
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
      .withPostState(state)
    val header = blockHeader(body, List.empty[ByteString], version, timestamp)

    unsignedBlockProto(body, header, List.empty[Justification])
  }

  //TODO: Decide on version number
  def fromInputFiles[F[_]: Monad: Capture: Log: Time](
      maybeBondsPath: Option[String],
      numValidators: Int,
      genesisPath: Path,
      maybeWalletsPath: Option[String],
      activeRuntime: Runtime)(implicit scheduler: Scheduler): F[BlockMessage] =
    for {
      bondsFile <- toFile[F](maybeBondsPath, genesisPath.resolve("bonds.txt"))
      _ <- bondsFile.fold[F[Unit]](maybeBondsPath.fold(().pure[F])(path =>
            Log[F].warn(
              s"CASPER: Specified bonds file $path does not exist. Falling back on generating random validators.")))(
            _ => ().pure[F])
      walletsFile <- toFile[F](maybeWalletsPath, genesisPath.resolve("wallets.txt"))
      wallets <- (walletsFile, maybeWalletsPath) match {
                  case (Some(file), _) => getWallets[F](file)
                  case (None, Some(path)) =>
                    Log[F]
                      .warn(
                        s"CASPER: Specified wallets file $path does not exist. No wallets will exist at genesis.")
                      .map(_ => Seq.empty[Wallet])
                  case (None, None) =>
                    Log[F]
                      .warn(
                        s"CASPER: No wallets file specified and no default file found. No wallets will exist at genesis.")
                      .map(_ => Seq.empty[Wallet])
                }
      bonds   <- getBonds[F](bondsFile, numValidators, genesisPath)
      now     <- Time[F].currentMillis
      initial = withoutContracts(bonds = bonds, timestamp = now, version = 0L)
      runtime = new SyncVar[Runtime]()
      genesis <- Capture[F].capture {
                  runtime.put(activeRuntime)
                  val (initStateHash, runtimeManager) = RuntimeManager.fromRuntime(runtime)
                  withContracts(initial, wallets, initStateHash, runtimeManager)
                }
    } yield genesis

  private def toFile[F[_]: Applicative: Log](maybePath: Option[String],
                                             defaultPath: Path): F[Option[File]] =
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

  private def getWallets[F[_]: Monad: Capture: Log](walletsFile: File): F[Seq[Wallet]] =
    for {
      maybeLines <- Capture[F].capture { Try(Source.fromFile(walletsFile).getLines().toList) }
      wallets <- maybeLines match {
                  case Success(lines) =>
                    lines
                      .traverse(Wallet.fromLine(_) match {
                        case Right(wallet) => wallet.some.pure[F]
                        case Left(errMsg) =>
                          Log[F]
                            .warn(s"CASPER: Error in parsing wallets file: $errMsg")
                            .map(_ => none[Wallet])
                      })
                      .map(_.flatten)
                  case Failure(ex) =>
                    Log[F]
                      .warn(
                        s"CASPER: Failed to read ${walletsFile.getAbsolutePath} for reason: ${ex.getMessage}")
                      .map(_ => Seq.empty[Wallet])
                }
    } yield wallets

  private def getBonds[F[_]: Monad: Capture: Log](bondsFile: Option[File],
                                                  numValidators: Int,
                                                  genesisPath: Path): F[Map[Array[Byte], Int]] =
    bondsFile match {
      case Some(file) =>
        Capture[F]
          .capture {
            Try {
              Source
                .fromFile(file)
                .getLines()
                .map(line => {
                  val Array(pk, stake) = line.trim.split(" ")
                  Base16.decode(pk) -> (stake.toInt)
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

  private def newValidators[F[_]: Monad: Capture: Log](
      numValidators: Int,
      genesisPath: Path): F[Map[Array[Byte], Int]] = {
    val keys         = Vector.fill(numValidators)(Ed25519.newKeyPair)
    val (_, pubKeys) = keys.unzip
    val bonds        = pubKeys.zip((1 to numValidators).toVector).toMap
    val genBondsFile = genesisPath.resolve(s"bonds.txt").toFile

    val skFiles = Capture[F].capture {
      genesisPath.toFile.mkdir()
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
      printer <- Capture[F].capture { new PrintWriter(genBondsFile) }
      _ <- Foldable[List].foldM[F, (Array[Byte], Int), Unit](bonds.toList, ()) {
            case (_, (pub, stake)) =>
              val pk = Base16.encode(pub)
              Log[F].info(s"Created validator $pk with bond $stake") *>
                Capture[F].capture { printer.println(s"$pk $stake") }
          }
      _ <- Capture[F].capture { printer.close() }
    } yield bonds
  }

}
