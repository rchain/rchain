package coop.rchain.node

import java.io.File
import java.security.cert.X509Certificate

import scala.util._
import cats._
import cats.data._
import cats.effect.Sync
import cats.implicits._
import coop.rchain.comm._
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.util.CertificateHelper
import coop.rchain.node.configuration.NodeConf
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler

object NodeEnvironment {

  class InitializationException(msg: String) extends RuntimeException

  def create[F[_]: Sync: Log](conf: NodeConf): F[NodeIdentifier] =
    for {
      dataDir <- Sync[F].delay(conf.storage.dataDir.toFile)
      _       <- canCreateDataDir(dataDir)
      _       <- haveAccessToDataDir(dataDir)
      _       <- Log[F].info(s"Using data dir: ${dataDir.getAbsolutePath}")
      _       <- transport.generateCertificateIfAbsent[F].apply(conf.tls)
      _       <- hasCertificate(conf)
      _       <- hasKey(conf)
      name    <- name(conf)
    } yield NodeIdentifier(name)

  private def isValid[F[_]: Sync](pred: Boolean, msg: String): F[Unit] =
    if (pred) Sync[F].raiseError(new RuntimeException(msg))
    else ().pure[F]

  private def name[F[_]: Sync](conf: NodeConf): F[String] = {
    val certificate: F[X509Certificate] =
      Sync[F]
        .delay(CertificateHelper.fromFile(conf.tls.certificatePath.toFile))
        .attempt
        .map(
          _.leftMap(
            e =>
              new InitializationException(s"Failed to read the X.509 certificate: ${e.getMessage}")
          )
        )
        .flatMap(e => Sync[F].fromEither(e))

    for {
      cert <- certificate
      pk   = cert.getPublicKey
      name <- certBase16(CertificateHelper.publicAddress(pk))
    } yield name
  }

  private def certBase16[F[_]: Sync](maybePubAddr: Option[Array[Byte]]): F[String] =
    maybePubAddr match {
      case Some(bytes) => Sync[F].delay(Base16.encode(bytes))
      case None =>
        Sync[F].fromEither(
          new InitializationException(
            "Certificate must contain a secp256r1 EC Public Key"
          ).asLeft[String]
        )
    }

  private def canCreateDataDir[F[_]: Sync](dataDir: File): F[Unit] = isValid(
    !dataDir.exists() && !dataDir.mkdir(),
    s"The data dir must be a directory and have read and write permissions:\\n${dataDir.getAbsolutePath}"
  )

  private def haveAccessToDataDir[F[_]: Sync](dataDir: File): F[Unit] = isValid(
    !dataDir.isDirectory || !dataDir.canRead || !dataDir.canWrite,
    s"The data dir must be a directory and have read and write permissions:\n${dataDir.getAbsolutePath}"
  )

  private def hasCertificate[F[_]: Sync](conf: NodeConf): F[Unit] = isValid(
    !conf.tls.certificatePath.toFile.exists(),
    s"Certificate file ${conf.tls.certificatePath} not found"
  )

  private def hasKey[F[_]: Sync](conf: NodeConf): F[Unit] = isValid(
    !conf.tls.keyPath.toFile.exists(),
    s"Secret key file ${conf.tls.certificatePath} not found"
  )
}
