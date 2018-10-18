package coop.rchain.casper

import cats.Applicative
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.Signature
import coop.rchain.casper.util.SignatureAlgorithms
import coop.rchain.shared.{Log, LogSource}

case class ValidatorIdentity(
    publicKey: Array[Byte],
    privateKey: Array[Byte],
    sigAlgorithm: String
) {
  def signature(data: Array[Byte]): Signature = {
    val sig = SignatureAlgorithms.lookup(sigAlgorithm)(data, privateKey)
    Signature(ByteString.copyFrom(publicKey), sigAlgorithm, ByteString.copyFrom(sig))
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
          .warn("No private key detected, cannot create validator identification.")
          .map(_ => none[ValidatorIdentity])
    }
}
