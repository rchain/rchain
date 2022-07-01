package coop.rchain.comm.transport

import cats.effect.Sync
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import coop.rchain.crypto.util.{CertificateHelper, CertificatePrinter}
import coop.rchain.shared.Log

import java.security.KeyPair
import scala.util.{Try, Using}

object GenerateCertificateIfAbsent {
  def apply[F[_]: Sync: Log]: F[GenerateCertificateIfAbsent[F]] =
    Sync[F].delay(new GenerateCertificateIfAbsent())

  /**
    * Generate certificate if not provided as option or in the data dir
    */
  def run[F[_]: Sync: Log](conf: TlsConf): F[Unit] =
    apply >>=
      (_.generateCertificate(conf)
        .whenA(!conf.customCertificateLocation && !conf.certificatePath.toFile.exists()))
}

class GenerateCertificateIfAbsent[F[_]: Sync: Log]() {

  def generateCertificate(tls: TlsConf): F[Unit] =
    for {
      _ <- Log[F].info(s"No certificate found at path ${tls.certificatePath}")
      _ <- Log[F].info("Generating a X.509 certificate for the node")
      // If there is a private key, use it for the certificate
      _ <- tls.keyPath.toFile
            .exists()
            .pure[F]
            .ifM(
              readKeyPair(tls),
              generateSecretKey(tls)
            )
    } yield ()

  def readKeyPair(tls: TlsConf): F[Unit] =
    for {
      _   <- Log[F].info(s"Using secret key ${tls.keyPath}")
      res <- Sync[F].delay(CertificateHelper.readKeyPair(tls.keyPath.toFile)).attempt
      _ <- res match {
            case Right(keyPair) =>
              Sync[F].fromTry(writeCert(tls, keyPair))
            case Left(e) =>
              Log[F].error(s"Invalid secret key: ${e.getMessage}")
          }
    } yield ()

  def generateSecretKey(tls: TlsConf): F[Unit] =
    for {
      _       <- Log[F].info("Generating a PEM secret key for the node")
      keyPair = CertificateHelper.generateKeyPair(tls.secureRandomNonBlocking)
      _       <- Sync[F].fromTry(writeCert(tls, keyPair))
      _       <- Sync[F].fromTry(writeKey(tls, keyPair))
    } yield ()

  def writeCert(tls: TlsConf, keyPair: KeyPair): Try[Unit] =
    Using(new java.io.PrintWriter(tls.certificatePath.toFile)) { fileWriter =>
      fileWriter.write(CertificatePrinter.print(CertificateHelper.generate(keyPair)))
    }

  def writeKey(tls: TlsConf, keyPair: KeyPair): Try[Unit] =
    Using(new java.io.PrintWriter(tls.keyPath.toFile)) { fileWriter =>
      fileWriter.write(CertificatePrinter.printPrivateKey(keyPair.getPrivate))
    }
}
