package coop.rchain.casper

import cats.Applicative
import cats.implicits._

import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.shared.{Log, LogSource}

case class ValidatorIdentity(publicKey: Array[Byte],
                             privateKey: Array[Byte],
                             sigAlgorithm: String) {

  //TODO: accept other signature algorithms
  val signFunction: (Array[Byte], Array[Byte]) => Array[Byte] = sigAlgorithm match {
    case "ed25519"   => Ed25519.sign _
    case "secp256k1" => Secp256k1.sign _
    case _           => throw new Exception(s"Unknown signature algorithm $sigAlgorithm")
  }
}

object ValidatorIdentity {
  private implicit val logSource: LogSource = LogSource(this.getClass)

  def fromConfig[F[_]: Applicative: Log](conf: CasperConf): F[Option[ValidatorIdentity]] =
    conf.privateKey match {
      case Some(privateKey) =>
        val publicKey =
          CasperConf.publicKey(conf.publicKey, conf.sigAlgorithm, privateKey)
        ValidatorIdentity(publicKey, privateKey, conf.sigAlgorithm).some.pure[F]

      case None =>
        Log[F]
          .warn("CASPER: No private key detected, cannot create validator identification.")
          .map(_ => none[ValidatorIdentity])
    }
}
