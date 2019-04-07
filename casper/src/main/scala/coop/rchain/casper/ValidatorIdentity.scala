package coop.rchain.casper

import java.io.{BufferedReader, FileReader}
import java.nio.file.Path

import cats.Applicative
import cats.effect.{Resource, Sync}
import cats.syntax.applicative._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.Signature
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.SignaturesAlg
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.shared.{Log, LogSource}

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
  private implicit val logSource: LogSource = LogSource(this.getClass)

  private def fileContent[F[_]: Sync](path: Path): F[String] = {
    val openFile = Sync[F].delay(new BufferedReader(new FileReader(path.toFile)))
    Resource.fromAutoCloseable(openFile).use(br => Sync[F].delay(br.readLine()))
  }

  private def createValidatorIdentity[F[_]: Applicative](
      conf: CasperConf,
      privateKeyBase16: String
  ) = {
    val privateKey     = PrivateKey(Base16.unsafeDecode(privateKeyBase16))
    val maybePublicKey = conf.publicKeyBase16.map(pk => PublicKey(Base16.unsafeDecode(pk)))

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
