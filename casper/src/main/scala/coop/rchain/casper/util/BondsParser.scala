package coop.rchain.casper.util

import cats.effect.{Async, Resource, Sync}
import cats.syntax.all._
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.shared.{Base16, Log}
import coop.rchain.models.syntax._
import fs2.io.file.{Files, Path}
import fs2.{io, text, Pipe, Stream}

object BondsParser {

  /**
    * Parser for bonds file used in genesis ceremony to set initial validators.
    *
    * TODO: Create Blocker scheduler for file operations. For now it's ok because it's used only once at genesis.
    *   Cats Effect 3 removed ContextShift and Blocker.
    *     - https://typelevel.org/cats-effect/docs/migration-guide#blocker
    */
  def parse[F[_]: Async: Log](bondsPath: Path): F[Map[PublicKey, Long]] = {
    def readLines =
      Files[F]
        .readAll(bondsPath)
        .through(text.utf8.decode)
        .through(text.lines)
        .filter(_.trim.nonEmpty)
        .evalMap { line =>
          val lineFormat = "<public_key> <stake>"
          val lineRegex  = raw"^([0-9a-fA-F]+) ([0-9]+)".r.unanchored

          // Line parser
          val pubKeyAndStake = tryWithMsg {
            line match { case lineRegex(fst, snd, _*) => (fst, snd) }
          }(failMsg = s"INVALID LINE FORMAT: `$lineFormat`, actual: `$line``")

          // Public key parser
          def publicKey(publicKeyStr: String) =
            publicKeyStr.decodeHex
              .map(PublicKey(_))
              .liftTo[F](new Exception(s"INVALID PUBLIC KEY: `$publicKeyStr`"))

          // Stake parser
          def stake(stakeStr: String) = tryWithMsg(stakeStr.toLong)(
            failMsg = s"INVALID STAKE `$stakeStr`. Please put positive number."
          )

          // Parse public key and stake
          pubKeyAndStake.flatMap(_.bitraverse(publicKey, stake))
        }
        .evalTap {
          case (pk, stake) => Log[F].info(s"Bond loaded ${Base16.encode(pk.bytes)} => $stake")
        }
        .compile
        .to(Map)
        .adaptErr {
          case ex: Throwable =>
            new Exception(s"FAILED PARSING BONDS FILE: $bondsPath\n$ex")
        }
    Resource.unit[F].use(_ => readLines)
  }

  def parse[F[_]: Async: Log](
      bondsPathStr: String,
      autogenShardSize: Int
  ): F[Map[PublicKey, Long]] = {
    val bondsPath = Path(bondsPathStr)

    def readLines =
      Files[F]
        .exists(bondsPath)
        .ifM(
          Log[F].info(s"Parsing bonds file $bondsPath.") >> parse(bondsPath),
          Log[F].warn(s"BONDS FILE NOT FOUND: $bondsPath. Creating file with random bonds.") >>
            newValidators[F](autogenShardSize, bondsPath.absolute)
        )
    Resource.unit[F].use(_ => readLines)
  }

  private def newValidators[F[_]: Async: Log](
      autogenShardSize: Int,
      bondsFilePath: Path
  ): F[Map[PublicKey, Long]] = {
    val genesisFolder = bondsFilePath.parent.get

    // Generate private/public key pairs
    val keys         = Vector.fill(autogenShardSize)(Secp256k1.newKeyPair)
    val (_, pubKeys) = keys.unzip
    val bonds        = pubKeys.iterator.zipWithIndex.toMap.mapValues(_.toLong + 1L)

    def toFile(filePath: Path): Pipe[F, String, Unit] =
      _.through(text.utf8.encode).through(Files[F].writeAll(filePath))

    // Write generated `<public_key>.sk` files with private key as content
    def writeSkFiles =
      Stream
        .fromIterator(keys.iterator, 1)
        .flatMap {
          case (privateKey, publicKey) =>
            val sk     = Base16.encode(privateKey.bytes)
            val pk     = Base16.encode(publicKey.bytes)
            val skFile = genesisFolder.resolve(s"$pk.sk")
            toFile(skFile)(Stream.emit(sk))
        }
        .compile
        .drain

    // Create bonds file with generated public keys
    def writeBondsFile = {
      val br = System.lineSeparator()
      val bondsStream = Stream
        .fromIterator(bonds.iterator, 1)
        .evalMap {
          case (publicKey, stake) =>
            val pk = Base16.encode(publicKey.bytes)
            Log[F].info(s"Bond generated $pk => $stake") *> s"$pk $stake$br".pure
        }
      toFile(bondsFilePath)(bondsStream).compile.drain
    }

    Files[F].createDirectories(genesisFolder) *>
      writeSkFiles *> writeBondsFile *> bonds.pure[F]

  }

  private def tryWithMsg[F[_]: Sync, A](f: => A)(failMsg: => String) =
    Sync[F].delay(f).adaptError { case _ => new Exception(failMsg) }
}
