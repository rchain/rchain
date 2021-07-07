package coop.rchain.casper.util

import cats.effect.{Blocker, ContextShift, Sync}
import cats.syntax.all._
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.shared.Log
import fs2.{io, text, Pipe, Stream}

import java.nio.file.Path

object BondsParser {

  /**
    * Parser for bonds file used in genesis ceremony to set initial validators.
    *
    * TODO: Create Blocker scheduler for file operations. For now it's ok because it's used only once at genesis.
    *   Cats Effect 3 removed ContextShift and Blocker.
    *     - https://typelevel.org/cats-effect/docs/migration-guide#blocker
    */
  def parse[F[_]: Sync: ContextShift: Log](bondsPath: Path): F[Map[PublicKey, Long]] = {
    def readLines(blocker: Blocker) =
      io.file
        .readAll[F](bondsPath, blocker, chunkSize = 4096)
        .through(text.utf8Decode)
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
            Base16
              .decode(publicKeyStr)
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
    Blocker[F].use(readLines)
  }

  def parse[F[_]: Sync: ContextShift: Log](
      bondsPathStr: String,
      autogenShardSize: Int
  ): F[Map[PublicKey, Long]] = {
    val bondsPath = Path.of(bondsPathStr)

    def readLines(blocker: Blocker) =
      io.file
        .exists(blocker, bondsPath)
        .ifM(
          Log[F].info(s"Parsing bonds file $bondsPath.") >> parse(bondsPath),
          Log[F].warn(s"BONDS FILE NOT FOUND: $bondsPath. Creating file with random bonds.") >>
            newValidators[F](autogenShardSize, bondsPath.toAbsolutePath)
        )
    Blocker[F].use(readLines)
  }

  private def newValidators[F[_]: Sync: ContextShift: Log](
      autogenShardSize: Int,
      bondsFilePath: Path
  ): F[Map[PublicKey, Long]] = {
    val genesisFolder = bondsFilePath.getParent

    // Generate private/public key pairs
    val keys         = Vector.fill(autogenShardSize)(Secp256k1.newKeyPair)
    val (_, pubKeys) = keys.unzip
    val bonds        = pubKeys.iterator.zipWithIndex.toMap.mapValues(_.toLong + 1L)

    def toFile(filePath: Path, blocker: Blocker): Pipe[F, String, Unit] =
      _.through(text.utf8Encode).through(io.file.writeAll(filePath, blocker))

    // Write generated `<public_key>.sk` files with private key as content
    def writeSkFiles(blocker: Blocker) =
      Stream
        .fromIterator(keys.iterator)
        .flatMap {
          case (privateKey, publicKey) =>
            val sk     = Base16.encode(privateKey.bytes)
            val pk     = Base16.encode(publicKey.bytes)
            val skFile = genesisFolder.resolve(s"$pk.sk")
            toFile(skFile, blocker)(Stream.emit(sk))
        }
        .compile
        .drain

    // Create bonds file with generated public keys
    def writeBondsFile(blocker: Blocker) = {
      val br = System.lineSeparator()
      val bondsStream = Stream
        .fromIterator(bonds.iterator)
        .evalMap {
          case (publicKey, stake) =>
            val pk = Base16.encode(publicKey.bytes)
            Log[F].info(s"Bond generated $pk => $stake") *> s"$pk $stake$br".pure
        }
      toFile(bondsFilePath, blocker)(bondsStream).compile.drain
    }

    // Write .sk files and bonds file
    Blocker[F].use { blocker =>
      io.file.createDirectories(blocker, genesisFolder) *>
        writeSkFiles(blocker) *> writeBondsFile(blocker) *> bonds.pure[F]
    }
  }

  private def tryWithMsg[F[_]: Sync, A](f: => A)(failMsg: => String) =
    Sync[F].delay(f).adaptError { case _ => new Exception(failMsg) }
}
