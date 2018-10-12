package coop.rchain.comm
import java.nio.file.Path

import coop.rchain.crypto.util.{CertificateHelper, CertificatePrinter}
import coop.rchain.shared.Iso

import scala.util.{Failure, Success, Try}

case class Tls(
    certificate: Path,
    key: Path,
    customCertificateLocation: Boolean,
    customKeyLocation: Boolean,
    secureRandomNonBlocking: Boolean
)

package object transport {
  def generateSertificateIfAbsent[A: Iso[?, Tls]](a: A) = {
    val tls = Iso[A, Tls].to(a)

    // Generate certificate if not provided as option or in the data dir
    if (!tls.customCertificateLocation
        && !tls.certificate.toFile.exists()) {
      println(s"No certificate found at path ${tls.certificate}")
      println("Generating a X.509 certificate for the node")

      import coop.rchain.shared.Resources._
      // If there is a private key, use it for the certificate
      if (tls.key.toFile.exists()) {
        println(s"Using secret key ${tls.key}")
        Try(CertificateHelper.readKeyPair(tls.key.toFile)) match {
          case Success(keyPair) =>
            withResource(new java.io.PrintWriter(tls.certificate.toFile)) {
              _.write(CertificatePrinter.print(CertificateHelper.generate(keyPair)))
            }
          case Failure(e) =>
            println(s"Invalid secret key: ${e.getMessage}")
        }
      } else {
        println("Generating a PEM secret key for the node")
        val keyPair = CertificateHelper.generateKeyPair(tls.secureRandomNonBlocking)
        withResource(new java.io.PrintWriter(tls.certificate.toFile)) { pw =>
          pw.write(CertificatePrinter.print(CertificateHelper.generate(keyPair)))
        }
        withResource(new java.io.PrintWriter(tls.key.toFile)) { pw =>
          pw.write(CertificatePrinter.printPrivateKey(keyPair.getPrivate))
        }
      }
    }
  }
}
