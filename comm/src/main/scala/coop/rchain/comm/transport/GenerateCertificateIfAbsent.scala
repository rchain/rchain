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

  def apply[A: Iso[?, Tls]](a: A): F[Unit] = {
    val tls = Iso[A, Tls].to(a)

    // Generate certificate if not provided as option or in the data dir
    if (!tls.customCertificateLocation
        && !tls.certificate.toFile.exists()) {

      generateCertificate(tls)
    } else ().pure[F]
  }

  def generateCertificate(tls: Tls): F[Unit] =
    for {
      _ <- info(s"No certificate found at path ${tls.certificate}")
      _ <- info("Generating a X.509 certificate for the node")
      // If there is a private key, use it for the certificate
      _ <- tls.key.toFile
            .exists()
            .pure[F]
            .ifM(
              readKeyPair(tls),
              generateSecretKey(tls)
            )
    } yield ()

  def readKeyPair(tls: Tls): F[Unit] =
    for {
      _   <- info(s"Using secret key ${tls.key}")
      res <- Sync[F].delay(certHelp.readKeyPair(tls.key.toFile)).attempt
      _ <- res match {
            case Right(keyPair) =>
              writeCert(tls, keyPair)
            case Left(e) =>
              error(s"Invalid secret key: ${e.getMessage}")
          }
    } yield ()

  def generateSecretKey(tls: Tls): F[Unit] =
    for {
      _       <- info("Generating a PEM secret key for the node")
      keyPair <- Sync[F].delay(certHelp.generateKeyPair(tls.secureRandomNonBlocking))
      _       <- writeCert(tls, keyPair)
      _       <- writeKey(tls, keyPair)
    } yield ()

  def writeCert(tls: Tls, keyPair: KeyPair): F[Unit] = {
    val certPw = Sync[F].delay(new java.io.PrintWriter(tls.certificate.toFile))

    fromAutoCloseable(certPw).use(
      pw =>
        Sync[F].delay(
          pw.write(certPrint.print(certHelp.generate(keyPair)))
        )
    )
  }

  def writeKey(tls: Tls, keyPair: KeyPair): F[Unit] = {
    val keyPw = Sync[F].delay(new java.io.PrintWriter(tls.key.toFile))

    fromAutoCloseable(keyPw).use(
      pw =>
        Sync[F].delay(
          pw.write(certPrint.printPrivateKey(keyPair.getPrivate))
        )
    )
  }
}
