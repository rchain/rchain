package coop.rchain.casper.util

import java.io.PrintWriter
import java.nio.file.{Path, Paths}

import cats.implicits._
import cats.{Foldable, Monad}
import cats.effect.Sync
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.blockstorage.util.io.{exists, SourceIO}
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.shared.{Log, LogSource}

import scala.util.{Failure, Success, Try}

object BondsParser {

  implicit private val logSource: LogSource = LogSource(this.getClass)

  private def parse[F[_]: Sync: RaiseIOError: Log](
      bondsPath: Path
  ): F[Map[PublicKey, Long]] =
    SourceIO
      .open[F](bondsPath)
      .use(_.getLines)
      .map { lines =>
        Try {
          lines
            .map(line => {
              val Array(pk, stake) = line.trim.split(" ")
              PublicKey(Base16.unsafeDecode(pk)) -> stake.toLong
            })
            .toMap
        }
      }
      .flatMap {
        case Success(bonds) =>
          bonds.toList
            .traverse_ {
              case (pk, stake) =>
                Log[F].info(s"Parsed validator ${Base16.encode(pk.bytes)} with bond $stake")
            }
            .as(bonds)
        case Failure(_) =>
          Sync[F].raiseError(new Exception(s"Bonds file $bondsPath cannot be parsed"))
      }

  def parse[F[_]: Sync: Log: RaiseIOError](
      maybeBondsPath: Option[String],
      defaultBondsPath: Path,
      autogenShardShize: Int,
      genesisPath: Path
  ): F[Map[PublicKey, Long]] =
    maybeBondsPath match {
      case Some(bondsPathStr) =>
        val bondsPath = Paths.get(bondsPathStr)
        exists(bondsPath).ifM(
          parse(bondsPath),
          Sync[F].raiseError(new Exception(s"Specified bonds file $bondsPath does not exist"))
        )
      case None =>
        exists(defaultBondsPath).ifM(
          Log[F].info(s"Using default file $defaultBondsPath") >> parse(defaultBondsPath),
          Log[F].warn(
            s"Bonds file was not specified and default bonds file does not exist. Falling back on generating random validators."
          ) >> newValidators[F](autogenShardShize, genesisPath)
        )
    }

  private def newValidators[F[_]: Monad: Sync: Log](
      autogenShardSize: Int,
      genesisPath: Path
  ): F[Map[PublicKey, Long]] = {
    val keys         = Vector.fill(autogenShardSize)(Secp256k1.newKeyPair)
    val (_, pubKeys) = keys.unzip
    val bonds        = pubKeys.zipWithIndex.toMap.mapValues(_.toLong + 1L)
    val genBondsFile = genesisPath.resolve(s"bonds.txt").toFile

    val skFiles =
      Sync[F].delay(genesisPath.toFile.mkdir()) >>
        Sync[F].delay {
          keys.foreach { //create files showing the secret key for each public key
            case (sec, pub) =>
              val sk      = Base16.encode(sec.bytes)
              val pk      = Base16.encode(pub.bytes)
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
      _ <- bonds.toList.traverse_ {
            case (pub, stake) =>
              val pk = Base16.encode(pub.bytes)
              Log[F].info(s"Created validator $pk with bond $stake") >> Sync[F].delay(
                printer.println(s"$pk $stake")
              )
          }
      _ <- Sync[F].delay { printer.close() }
    } yield bonds
  }

}
