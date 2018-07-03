package coop.rchain.casper.genesis

import cats.{Applicative, Foldable, Monad}
import cats.implicits._

import com.google.protobuf.ByteString

import coop.rchain.catscontrib._
import coop.rchain.casper.genesis.contracts.{Rev, Wallet}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil.{blockHeader, termDeploy, unsignedBlockProto}
import coop.rchain.casper.util.Sorting
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
    )

    val Right(checkpoint) = runtimeManager.computeState(startHash, blessedTerms)
    val stateHash         = ByteString.copyFrom(checkpoint.root.bytes.toArray)

    val stateWithContracts = for {
      bd <- initial.body
      ps <- bd.postState
    } yield ps.withTuplespace(stateHash)
    val version   = initial.header.get.version
    val timestamp = initial.header.get.timestamp

    //TODO: add comm reductions
    val body   = Body(postState = stateWithContracts, newCode = blessedTerms.map(termDeploy))
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
  //TODO: Include wallets input file
  def fromBondsFile[F[_]: Monad: Capture: Log: Time](
      maybePath: Option[String],
      numValidators: Int,
      validatorsPath: Path)(implicit scheduler: Scheduler): F[BlockMessage] =
    for {
      bondsFile <- toFile[F](maybePath, validatorsPath)
      bonds     <- getBonds[F](bondsFile, numValidators, validatorsPath)
      now       <- Time[F].currentMillis
      initial   = withoutContracts(bonds = bonds, timestamp = now, version = 0L)
      genesis <- createRuntime[F].flatMap {
                  case (startHash, runtimeManager, runtime) =>
                    val result =
                      withContracts(initial, Seq.empty[Wallet], startHash, runtimeManager)
                    result.pure[F] <* closeRuntime(runtime)(Capture[F])
                }
    } yield genesis

  //TODO: remove in favour of having a runtime passed in
  private def createRuntime[F[_]: Capture]: F[(StateHash, RuntimeManager, SyncVar[Runtime])] =
    Capture[F].capture {
      val storageSize     = 1024L * 1024
      val storageLocation = Files.createTempDirectory(s"casper-genesis-runtime")
      val runtime         = new SyncVar[Runtime]()
      runtime.put(Runtime.create(storageLocation, storageSize))
      val (initStateHash, runtimeManager) = RuntimeManager.fromRuntime(runtime)
      (initStateHash, runtimeManager, runtime)
    }

  private def closeRuntime[F[_]: Capture](runtime: SyncVar[Runtime]): F[Unit] = Capture[F].capture {
    val active = runtime.take()
    active.close()
  }

  private def toFile[F[_]: Applicative: Log](maybePath: Option[String],
                                             validatorsPath: Path): F[Option[File]] =
    maybePath match {
      case Some(path) =>
        val f = new File(path)
        if (f.exists) f.some.pure[F]
        else {
          Log[F].warn(
            s"Specified bonds file $path does not exist. Falling back on generating random validators."
          ) *> none[File].pure[F]
        }

      case None =>
        val default = validatorsPath.resolve("bonds.txt").toFile
        if (default.exists) {
          Log[F].info(
            s"Found default bonds file ${default.getPath}."
          ) *> default.some.pure[F]
        } else none[File].pure[F]
    }

  private def getBonds[F[_]: Monad: Capture: Log](bondsFile: Option[File],
                                                  numValidators: Int,
                                                  validatorsPath: Path): F[Map[Array[Byte], Int]] =
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
              ) *> newValidators[F](numValidators, validatorsPath)
          }
      case None => newValidators[F](numValidators, validatorsPath)
    }

  private def newValidators[F[_]: Monad: Capture: Log](
      numValidators: Int,
      validatorsPath: Path): F[Map[Array[Byte], Int]] = {
    val keys         = Vector.fill(numValidators)(Ed25519.newKeyPair)
    val (_, pubKeys) = keys.unzip
    val bonds        = pubKeys.zip((1 to numValidators).toVector).toMap
    val genBondsFile = validatorsPath.resolve(s"bonds.txt").toFile

    val skFiles = Capture[F].capture {
      validatorsPath.toFile.mkdir()
      keys.foreach { //create files showing the secret key for each public key
        case (sec, pub) =>
          val sk      = Base16.encode(sec)
          val pk      = Base16.encode(pub)
          val skFile  = validatorsPath.resolve(s"$pk.sk").toFile
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
