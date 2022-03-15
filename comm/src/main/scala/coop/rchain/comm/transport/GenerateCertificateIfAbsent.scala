package coop.rchain.comm.transport

import java.security.KeyPair

import cats.effect.Resource.fromAutoCloseable
import cats.effect.Sync

import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._

import coop.rchain.crypto.util.{CertificateHelper => certHelp, CertificatePrinter => certPrint}
import coop.rchain.shared.{Iso, Log}

import scala.language.higherKinds

class GenerateCertificateIfAbsent[F[_]: Sync](implicit log: Log[F]) {
  import log.{error, info}

  def apply[A: Iso[*, TlsConf]](a: A): F[Unit] = {
    val tls = Iso[A, TlsConf].to(a)

    // Generate certificate if not provided as option or in the data dir
    if (!tls.customCertificateLocation
        && !tls.certificatePath.toFile.exists()) {

      generateCertificate(tls)
    } else ().pure[F]
  }

  def generateCertificate(tls: TlsConf): F[Unit] =
    for {
      _ <- info(s"No certificate found at path ${tls.certificatePath}")
      _ <- info("Generating a X.509 certificate for the node")
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
      _   <- info(s"Using secret key ${tls.keyPath}")
      res <- Sync[F].delay(certHelp.readKeyPair(tls.keyPath.toFile)).attempt
      _ <- res match {
            case Right(keyPair) =>
              writeCert(tls, keyPair)
            case Left(e) =>
              error(s"Invalid secret key: ${e.getMessage}")
          }
    } yield ()

  def generateSecretKey(tls: TlsConf): F[Unit] =
    for {
      _       <- info("Generating a PEM secret key for the node")
      keyPair <- Sync[F].delay(certHelp.generateKeyPair(tls.secureRandomNonBlocking))
      _       <- writeCert(tls, keyPair)
      _       <- writeKey(tls, keyPair)
    } yield ()

  def writeCert(tls: TlsConf, keyPair: KeyPair): F[Unit] = {
    val certPw = Sync[F].delay(new java.io.PrintWriter(tls.certificatePath.toFile))

    fromAutoCloseable(certPw).use(
      pw =>
        Sync[F].delay(
          pw.write(certPrint.print(certHelp.generate(keyPair)))
        )
    )
  }

  def writeKey(tls: TlsConf, keyPair: KeyPair): F[Unit] = {
    val keyPw = Sync[F].delay(new java.io.PrintWriter(tls.keyPath.toFile))

    fromAutoCloseable(keyPw).use(
      pw =>
        Sync[F].delay(
          pw.write(certPrint.printPrivateKey(keyPair.getPrivate))
        )
    )
  }
}
