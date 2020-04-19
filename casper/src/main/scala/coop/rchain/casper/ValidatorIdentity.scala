package coop.rchain.casper

import cats.Applicative
import cats.effect.Sync
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.Signature
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.{Secp256k1, SignaturesAlg}
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.shared.{EnvVars, Log, LogSource}

import scala.language.higherKinds
import scala.util.Try

final case class ValidatorIdentity(
    publicKey: PublicKey,
    privateKey: PrivateKey,
    sigAlgorithm: String
) {
  def signature(data: Array[Byte]): Signature = {
    val sig = SignaturesAlg(sigAlgorithm).map(_.sign(data, privateKey)).get
    Signature(ByteString.copyFrom(publicKey.bytes), sigAlgorithm, ByteString.copyFrom(sig))
  }
}

object ValidatorIdentity {
  private val RNodeValidatorPasswordEnvVar  = "RNODE_VALIDATOR_PASSWORD"
  implicit private val logSource: LogSource = LogSource(this.getClass)

  def getEnvVariablePassword[F[_]: Sync: EnvVars]: F[String] =
    EnvVars[F].get(RNodeValidatorPasswordEnvVar).flatMap {
      case Some(password) =>
        password.pure[F]
      case None =>
        Sync[F].raiseError(
          new Exception(s"Environment variable $RNodeValidatorPasswordEnvVar is unspecified")
        )
    }

  private def createValidatorIdentity[F[_]: Applicative](
      conf: CasperConf,
      privateKey: PrivateKey
  ): F[Option[ValidatorIdentity]] = {
    val maybePublicKey = conf.validatorPublicKey.map(pk => PublicKey(Base16.unsafeDecode(pk)))

    ValidatorIdentity(
      publicKey(maybePublicKey, Secp256k1.name, privateKey),
      privateKey,
      Secp256k1.name
    ).some.pure[F]
  }

  def fromPrivateKey[F[_]: Sync: EnvVars: Log](conf: CasperConf): F[Option[ValidatorIdentity]] =
    conf.validatorPrivateKey match {
      case Some(privateKeyBase16) =>
        createValidatorIdentity(conf, PrivateKey(Base16.unsafeDecode(privateKeyBase16)))
      case None =>
        Log[F]
          .warn("No private key detected, cannot create validator identification.")
          .map(_ => none[ValidatorIdentity])
    }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def publicKey(
      givenPublicKey: Option[PublicKey],
      sigAlgorithm: String,
      privateKey: PrivateKey
  ): PublicKey = {

    val maybeInferred = sigAlgorithm match {
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
