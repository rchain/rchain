package coop.rchain.node

import java.io.{File, FileInputStream}
import java.security.AlgorithmParameters
import java.security.cert.{CertificateFactory, X509Certificate}
import java.security.interfaces.ECPublicKey
import java.security.spec._

import coop.rchain.crypto.hash.Keccak256

object CertificateHelper {

  lazy val Secp256k1: ParameterSpec = {
    val ap = AlgorithmParameters.getInstance("EC", "SunEC")
    ap.init(new ECGenParameterSpec("secp256k1"))
    ParameterSpec(ap.getParameterSpec(classOf[ECParameterSpec]))
  }

  def isSecp256k1(certificate: X509Certificate): Boolean =
    certificate.getPublicKey match {
      case p: ECPublicKey =>
        ParameterSpec(p.getParams) == Secp256k1
      case _ => false
    }

  def publicAddress(certificate: X509Certificate): Option[String] =
    certificate.getPublicKey match {
      case p: ECPublicKey if isSecp256k1(certificate) =>
        val publicKey      = Array.ofDim[Byte](64)
        val x: Array[Byte] = p.getW.getAffineX.toByteArray
        val y: Array[Byte] = p.getW.getAffineY.toByteArray
        x.copyToArray(publicKey)
        y.copyToArray(publicKey, 64 - y.length)
        Some(publicAddress(publicKey))
      case _ => None
    }

  private def publicAddress(input: Array[Byte]): String =
    Keccak256.hash(input).drop(12).map("%02x".format(_)).mkString

  def from(certFilePath: String): X509Certificate =
    fromFile(new File(certFilePath))

  def fromFile(certFile: File): X509Certificate = {
    val cf = CertificateFactory.getInstance("X.509")
    val is = new FileInputStream(certFile)
    cf.generateCertificate(is).asInstanceOf[X509Certificate]
  }

}

case class ParameterSpec(
    curve: EllipticCurve,
    generator: ECPoint,
    order: BigInt,
    cofactor: Int
)

case object ParameterSpec {
  def apply(ecParamSpec: ECParameterSpec): ParameterSpec =
    ParameterSpec(ecParamSpec.getCurve,
                  ecParamSpec.getGenerator,
                  ecParamSpec.getOrder,
                  ecParamSpec.getCofactor)
}
