package coop.rchain.node

import scala.util._

import coop.rchain.comm.NodeIdentifier
import coop.rchain.comm.transport.{CertificateHelper, CertificatePrinter}
import coop.rchain.crypto.codec.Base16
import coop.rchain.node.configuration.Configuration

import monix.eval.Task

object NodeEnvironment {

  def create(conf: Configuration): Task[NodeIdentifier] =
    Task.delay {
      val dataDirFile = conf.server.dataDir.toFile

      if (!dataDirFile.exists()) {
        if (!dataDirFile.mkdir()) {
          println(
            s"The data dir must be a directory and have read and write permissions:\n${dataDirFile.getAbsolutePath}"
          )
          System.exit(-1)
        }
      }

      // Check if data_dir has read/write access
      if (!dataDirFile.isDirectory || !dataDirFile.canRead || !dataDirFile.canWrite) {
        println(
          s"The data dir must be a directory and have read and write permissions:\n${dataDirFile.getAbsolutePath}"
        )
        System.exit(-1)
      }

      println(s"Using data_dir: ${dataDirFile.getAbsolutePath}")

      // Generate certificate if not provided as option or in the data dir
      if (!conf.tls.customCertificateLocation
          && !conf.tls.certificate.toFile.exists()) {
        println(s"No certificate found at path ${conf.tls.certificate}")
        println("Generating a X.509 certificate for the node")

        import coop.rchain.shared.Resources._
        // If there is a key, use it for the certificate
        if (conf.tls.key.toFile.exists()) {
          println(s"Using secret key ${conf.tls.key}")
          Try(CertificateHelper.readKeyPair(conf.tls.key.toFile)) match {
            case Success(keyPair) =>
              withResource(new java.io.PrintWriter(conf.tls.certificate.toFile)) {
                _.write(CertificatePrinter.print(CertificateHelper.generate(keyPair)))
              }
            case Failure(e) =>
              println(s"Invalid secret key: ${e.getMessage}")
          }
        } else {
          println("Generating a PEM secret key for the node")
          val keyPair = CertificateHelper.generateKeyPair(conf.tls.secureRandomNonBlocking)
          withResource(new java.io.PrintWriter(conf.tls.certificate.toFile)) { pw =>
            pw.write(CertificatePrinter.print(CertificateHelper.generate(keyPair)))
          }
          withResource(new java.io.PrintWriter(conf.tls.key.toFile)) { pw =>
            pw.write(CertificatePrinter.printPrivateKey(keyPair.getPrivate))
          }
        }
      }

      if (!conf.tls.certificate.toFile.exists()) {
        println(s"Certificate file ${conf.tls.certificate} not found")
        System.exit(-1)
      }

      if (!conf.tls.key.toFile.exists()) {
        println(s"Secret key file ${conf.tls.certificate} not found")
        System.exit(-1)
      }

      val name: String = {
        val publicKey = Try(CertificateHelper.fromFile(conf.tls.certificate.toFile)) match {
          case Success(c) => Some(c.getPublicKey)
          case Failure(e) =>
            println(s"Failed to read the X.509 certificate: ${e.getMessage}")
            System.exit(1)
            None
          case _ => None
        }

        val publicKeyHash = publicKey
          .flatMap(CertificateHelper.publicAddress)
          .map(Base16.encode)

        if (publicKeyHash.isEmpty) {
          println("Certificate must contain a secp256r1 EC Public Key")
          System.exit(1)
        }

        publicKeyHash.get
      }

      NodeIdentifier(name)
    }
}
