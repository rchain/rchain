package coop.rchain.casper

import java.nio.file.Path

import cats.Monad
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.catscontrib.Capture
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.shared.{Log, LogSource}

import scala.concurrent.duration.FiniteDuration
import scala.io.Source
import scala.util.{Failure, Success, Try}

case class CasperConf(
    publicKeyBase16: Option[String],
    privateKey: Option[Either[String, Path]],
    sigAlgorithm: String,
    bondsFile: Option[String],
    knownValidatorsFile: Option[String],
    numValidators: Int,
    genesisPath: Path,
    walletsFile: Option[String],
    minimumBond: Long,
    maximumBond: Long,
    hasFaucet: Boolean,
    requiredSigs: Int,
    shardId: String,
    createGenesis: Boolean,
    approveGenesis: Boolean,
    approveGenesisInterval: FiniteDuration,
    approveGenesisDuration: FiniteDuration,
    deployTimestamp: Option[Long]
)

object CasperConf {
  private implicit val logSource: LogSource = LogSource(this.getClass)

  def parseValidatorsFile[F[_]: Monad: Capture: Log](
      knownValidatorsFile: Option[String]
  ): F[Set[ByteString]] =
    knownValidatorsFile match {
      //TODO: Add default set? Throw error?
      case None => Set.empty[ByteString].pure[F]

      case Some(file) =>
        Capture[F]
          .capture {
            Try(
              Source
                .fromFile(file)
                .getLines()
                .map(line => ByteString.copyFrom(Base16.decode(line)))
                .toSet
            )
          }
          .flatMap {
            case Success(validators) => validators.pure[F]

            case Failure(ex) =>
              Log[F]
                .error(s"Error while parsing known validators file; $ex: ${ex.getMessage}")
                .map[Set[ByteString]](_ => throw ex)
          }
    }

  def publicKey(
      givenPublicKey: Option[Array[Byte]],
      sigAlgorithm: String,
      privateKey: Array[Byte]
  ): Array[Byte] = {

    val maybeInferred = sigAlgorithm match {
      case "ed25519" =>
        Try(Ed25519.toPublic(privateKey)).toOption

      case "secp256k1" =>
        Try(Secp256k1.toPublic(privateKey)).toOption

      case _ => None
    }

    (maybeInferred, givenPublicKey) match {
      case (Some(k1), Some(k2)) =>
        if (keysMatch(k1, k2)) k1
        else throw new Exception("Public key not compatible with given private key!")

      case (Some(k1), None) => k1

      //TODO: Should this case be an error?
      //Will all supported algorithms be able to infer the public key from private?
      case (None, Some(k2)) => k2

      case (None, None) =>
        throw new Exception("Public key must be specified, cannot infer from private key.")
    }
  }

  private def keysMatch(k1: Array[Byte], k2: Array[Byte]): Boolean =
    k1.zip(k2).forall { case (x, y) => x == y }
}
