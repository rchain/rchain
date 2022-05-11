package coop.rchain.casper

import cats.Applicative
import cats.effect.Sync
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.crypto.signatures.{Secp256k1, SignaturesAlg}
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.models.syntax._
import coop.rchain.shared.{EnvVars, Log, LogSource}

final case class ValidatorIdentity(
    publicKey: PublicKey,
    privateKey: PrivateKey,
    sigAlgorithm: String
) {
  def signature(data: Array[Byte]): Signature = {
    val sig = SignaturesAlg(sigAlgorithm).map(_.sign(data, privateKey)).get
    Signature(publicKey, sigAlgorithm, sig)
  }

  def signBlock(
      block: BlockMessage
  ): BlockMessage = {

    val sender = ByteString.copyFrom(publicKey.bytes)

    // Hash should include sigAlgorithm and sender
    val b = block.copy(sigAlgorithm = sigAlgorithm, sender = sender)

    val blockHash = ProtoUtil.hashBlock(b)

    val sig = signature(blockHash.toByteArray).signature.toByteString

    b.copy(sig = sig, blockHash = blockHash)
  }
}

final case class Signature(pk: PublicKey, sigAlgorithm: String, signature: Array[Byte])

object ValidatorIdentity {
  private val RNodeValidatorPasswordEnvVar  = "RNODE_VALIDATOR_PASSWORD"
  implicit private val logSource: LogSource = LogSource(this.getClass)

  def apply(
      privateKey: PrivateKey
  ): ValidatorIdentity = {
    val publicKey = Secp256k1.toPublic(privateKey)

    ValidatorIdentity(
      publicKey,
      privateKey,
      Secp256k1.name
    )
  }

  def getEnvVariablePassword[F[_]: Sync: EnvVars]: F[String] =
    EnvVars[F].get(RNodeValidatorPasswordEnvVar) >>= (
      _.liftTo(new Exception(s"Environment variable $RNodeValidatorPasswordEnvVar is unspecified"))
    )

  def fromHex(privKeyHex: String): Option[ValidatorIdentity] =
    privKeyHex.decodeHex.map(PrivateKey(_)).map(ValidatorIdentity(_))

  def fromPrivateKeyWithLogging[F[_]: Applicative: Log](
      privKey: Option[String]
  ): F[Option[ValidatorIdentity]] =
    privKey
      .map(fromHex)
      .fold(
        Log[F]
          .warn("No private key detected, cannot create validator identification.")
          .as(none[ValidatorIdentity])
      )(_.pure)
}
