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

  def parse[F[_]: Sync: RaiseIOError: Log](
      bondsPath: Path
  ): F[Map[PublicKey, Long]] =
    SourceIO
      .open[F](bondsPath)
      .use(_.getLines)
      .flatTap(
        bondsStrs =>
          Sync[F]
            .raiseError(
              new Exception(
                s"BONDS FILE $bondsPath IS EMPTY. Please fill it or remove so populated bonds file is created on startup."
              )
            )
            .whenA(bondsStrs.isEmpty)
      )
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
                Log[F].info(s"Bond loaded ${Base16.encode(pk.bytes)} => $stake")
            }
            .as(bonds)
        case Failure(e) =>
          Sync[F].raiseError(
            new Exception(s"FAILED PARSING BONDS FILE $bondsPath: ${e}")
          )
      }

  def parse[F[_]: Sync: Log: RaiseIOError](
      bondsPathStr: String,
      autogenShardSize: Int
  ): F[Map[PublicKey, Long]] = {
    val bondsPath = Paths.get(bondsPathStr)
    exists(bondsPath).ifM(
      Log[F].info(s"Parsing bonds file ${bondsPath}.") >> parse(bondsPath),
      Log[F].warn(s"BONDS FILE NOT FOUND: ${bondsPath}. Creating file with random bonds.") >>
        newValidators[F](autogenShardSize, Path.of(bondsPathStr).toAbsolutePath)
    )
  }

  private def newValidators[F[_]: Monad: Sync: Log](
      autogenShardSize: Int,
      bondsFilePath: Path
  ): F[Map[PublicKey, Long]] = {
    val genesisFolder = bondsFilePath.getParent
    val keys          = Vector.fill(autogenShardSize)(Secp256k1.newKeyPair)
    val (_, pubKeys)  = keys.unzip
    val bonds         = pubKeys.zipWithIndex.toMap.mapValues(_.toLong + 1L)
    val genBondsFile  = bondsFilePath.toFile

    val skFiles =
      Sync[F].delay(genesisFolder.toFile.mkdirs()) >>
        Sync[F].delay {
          keys.foreach { //create files showing the secret key for each public key
            case (sec, pub) =>
              val sk      = Base16.encode(sec.bytes)
              val pk      = Base16.encode(pub.bytes)
              val skFile  = genesisFolder.resolve(s"$pk.sk").toFile
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
              Log[F].info(s"Bond generated $pk => $stake") >> Sync[F].delay(
                printer.println(s"$pk $stake")
              )
          }
      _ <- Sync[F].delay { printer.close() }
    } yield bonds
  }

}
