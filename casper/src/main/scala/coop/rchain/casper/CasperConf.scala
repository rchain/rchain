package coop.rchain.casper

import java.nio.file.Path

import cats.Monad
import cats.effect._
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.shared.{Log, LogSource}

import scala.concurrent.duration.FiniteDuration
import scala.io.Source
import scala.util.{Failure, Success, Try}

final case class CasperConf(
    publicKeyBase16: Option[String],
    privateKey: Option[Either[String, Path]],
    bondsFile: Option[String],
    knownValidatorsFile: Option[String],
    numValidators: Int,
    genesisPath: Path,
    walletsFile: Option[String],
    minimumBond: Long,
    maximumBond: Long,
    epochLength: Int,
    quarantineLength: Int,
    numberOfActiveValidators: Int,
    casperLoopInterval: Int,
    requestedBlocksTimeout: Int,
    requiredSigs: Int,
    shardId: String,
    createGenesis: Boolean,
    approveGenesis: Boolean,
    approveGenesisInterval: FiniteDuration,
    approveGenesisDuration: FiniteDuration,
    deployTimestamp: Option[Long],
    finalizationRate: Int,
    maxNumberOfParents: Int,
    forkChioceStaleThreshold: FiniteDuration,
    forkChioceCheckIfStaleInterval: FiniteDuration,
    maxParentDepthOpt: Option[Int]
)

object CasperConf {
  implicit private val logSource: LogSource = LogSource(this.getClass)

  @SuppressWarnings(Array("org.wartremover.warts.Throw")) // TODO remove throw
  def parseValidatorsFile[F[_]: Monad: Sync: Log](
      knownValidatorsFile: Option[String]
  ): F[Set[ByteString]] =
    knownValidatorsFile match {
      //TODO: Add default set? Throw error?
      case None => Set.empty[ByteString].pure[F]

      case Some(file) =>
        Sync[F]
          .delay {
            Try(
              Source
                .fromFile(file)
                .getLines()
                .map(line => ByteString.copyFrom(Base16.unsafeDecode(line)))
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

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def publicKey(
      givenPublicKey: Option[PublicKey],
      sigAlgorithm: String,
      privateKey: PrivateKey
  ): PublicKey = {

    val maybeInferred = sigAlgorithm match {
      case "ed25519" =>
        Try(Ed25519.toPublic(privateKey)).toOption

      case "secp256k1" =>
        Try(Secp256k1.toPublic(privateKey)).toOption

      case _ => None
    }

    (maybeInferred, givenPublicKey) match {
      case (Some(k1), Some(k2)) =>
        if (keysMatch(k1.bytes, k2.bytes)) k1
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
