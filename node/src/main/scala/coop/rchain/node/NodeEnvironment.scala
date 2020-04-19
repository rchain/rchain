package coop.rchain.node

import java.io.File
import java.security.cert.X509Certificate

import scala.util._
import cats._
import cats.data._
import cats.implicits._
import coop.rchain.comm._
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.util.CertificateHelper
import coop.rchain.node.configuration.{NodeConf}
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler

object NodeEnvironment {

  class InitializationException(msg: String) extends RuntimeException

  def create(conf: NodeConf)(implicit log: Log[Task]): Task[NodeIdentifier] =
    for {
      dataDir <- Task.delay(conf.storage.dataDir.toFile)
      _       <- canCreateDataDir(dataDir)
      _       <- haveAccessToDataDir(dataDir)
      _       <- log.info(s"Using data dir: ${dataDir.getAbsolutePath}")
      _       <- transport.generateCertificateIfAbsent[Task].apply(conf.tls)
      _       <- hasCertificate(conf)
      _       <- hasKey(conf)
      name    <- name(conf)
    } yield NodeIdentifier(name)

  private def isValid(pred: Boolean, msg: String): Task[Unit] =
    if (pred) Task.raiseError(new RuntimeException(msg))
    else Task.unit

  private def name(conf: NodeConf): Task[String] = {
    val certificate: Task[X509Certificate] =
      Task
        .delay(CertificateHelper.fromFile(conf.tls.certificatePath.toFile))
        .attempt
        .map(
          _.leftMap(
            e =>
              new InitializationException(s"Failed to read the X.509 certificate: ${e.getMessage}")
          )
        )
        .flatMap(e => Task.fromEither(e))

    for {
      cert <- certificate
      pk   = cert.getPublicKey
      name <- certBase16(CertificateHelper.publicAddress(pk))
    } yield name
  }

  private def certBase16(maybePubAddr: Option[Array[Byte]]): Task[String] =
    maybePubAddr match {
      case Some(bytes) => Base16.encode(bytes).pure[Task]
      case None =>
        Task.fromEither(
          new InitializationException(
            "Certificate must contain a secp256r1 EC Public Key"
          ).asLeft[String]
        )
    }

  private def canCreateDataDir(dataDir: File): Task[Unit] = isValid(
    !dataDir.exists() && !dataDir.mkdir(),
    s"The data dir must be a directory and have read and write permissions:\\n${dataDir.getAbsolutePath}"
  )

  private def haveAccessToDataDir(dataDir: File): Task[Unit] = isValid(
    !dataDir.isDirectory || !dataDir.canRead || !dataDir.canWrite,
    s"The data dir must be a directory and have read and write permissions:\n${dataDir.getAbsolutePath}"
  )

  private def hasCertificate(conf: NodeConf): Task[Unit] = isValid(
    !conf.tls.certificatePath.toFile.exists(),
    s"Certificate file ${conf.tls.certificatePath} not found"
  )

  private def hasKey(conf: NodeConf): Task[Unit] = isValid(
    !conf.tls.keyPath.toFile.exists(),
    s"Secret key file ${conf.tls.certificatePath} not found"
  )
}
