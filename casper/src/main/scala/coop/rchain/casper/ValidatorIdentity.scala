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
    val maybePublicKey = conf.publicKeyBase16.map(pk => PublicKey(Base16.unsafeDecode(pk)))

    val publicKey =
      CasperConf.publicKey(maybePublicKey, Secp256k1.name, privateKey)

    ValidatorIdentity(publicKey, privateKey, Secp256k1.name).some.pure[F]
  }

  def fromConfig[F[_]: Sync: EnvVars: Log](conf: CasperConf): F[Option[ValidatorIdentity]] =
    conf.privateKey match {
      case Some(Left(privateKeyBase16)) =>
        createValidatorIdentity(conf, PrivateKey(Base16.unsafeDecode(privateKeyBase16)))
      case Some(Right(privateKeyPath)) =>
        for {
          password          <- getEnvVariablePassword
          privateKey        <- Secp256k1.parsePemFile(privateKeyPath, password)
          validatorIdentity <- createValidatorIdentity(conf, privateKey)
        } yield validatorIdentity
      case None =>
        Log[F]
          .warn("No private key detected, cannot create validator identification.")
          .map(_ => none[ValidatorIdentity])
    }
}
