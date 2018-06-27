package coop.rchain.casper.genesis

import cats.{Applicative, Foldable, Monad}
import cats.implicits._

import com.google.protobuf.ByteString

import coop.rchain.catscontrib.Capture
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil.{blockHeader, unsignedBlockProto}
import coop.rchain.casper.util.Sorting
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.shared.{Log, LogSource}

import java.io.{File, PrintWriter}
import java.nio.file.Path

import scala.io.Source
import scala.util.{Failure, Success, Try}

object Genesis {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def fromBondsFile[F[_]: Monad: Capture: Log](maybePath: Option[String],
                                               numValidators: Int,
                                               validatorsPath: Path): F[BlockMessage] =
    for {
      bondsFile <- toFile[F](maybePath, validatorsPath)
      bonds     <- getBonds[F](bondsFile, numValidators, validatorsPath)
    } yield fromBonds(bonds)

  // TODO: Extract hard-coded version and timestamp
  def fromBonds(bonds: Map[Array[Byte], Int]): BlockMessage = {
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
    val header = blockHeader(body, List.empty[ByteString], 0L, 0L)

    unsignedBlockProto(body, header, List.empty[Justification])
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
