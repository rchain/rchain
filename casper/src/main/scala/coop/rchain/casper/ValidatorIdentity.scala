package coop.rchain.casper

import java.io.{BufferedReader, FileReader}
import java.nio.file.Path

import cats.Applicative
import cats.effect.{Resource, Sync}
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.option._
import cats.syntax.either._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.Signature
import coop.rchain.casper.util.SignatureAlgorithms
import coop.rchain.crypto.codec.Base16
import coop.rchain.shared.{Log, LogSource}

import scala.language.higherKinds

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

  private def fileContent[F[_]: Sync](path: Path): F[String] = {
    val openFile = Sync[F].delay(new BufferedReader(new FileReader(path.toFile)))
    Resource.fromAutoCloseable(openFile).use(br => Sync[F].delay(br.readLine()))
  }

  private def createValidatorIdentity[F[_]: Applicative](
      conf: CasperConf,
      privateKeyBase16: String
  ) = {
    val privateKey     = Base16.decode(privateKeyBase16)
    val maybePublicKey = conf.publicKeyBase16.map(Base16.decode)

    val publicKey =
      CasperConf.publicKey(maybePublicKey, conf.sigAlgorithm, privateKey)

    ValidatorIdentity(publicKey, privateKey, conf.sigAlgorithm).some.pure[F]
  }

  def fromConfig[F[_]: Sync: Log](conf: CasperConf): F[Option[ValidatorIdentity]] =
    conf.privateKey match {
      case Some(key) =>
        key.map(fileContent[F]).leftMap(_.pure[F]).merge >>= (createValidatorIdentity(conf, _))
      case None =>
        Log[F]
          .warn("No private key detected, cannot create validator identification.")
          .map(_ => none[ValidatorIdentity])
    }
}
