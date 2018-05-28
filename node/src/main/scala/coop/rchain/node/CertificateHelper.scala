package coop.rchain.node

import java.io.{File, FileInputStream}
import java.security.{AlgorithmParameters, PublicKey}
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

  def isSecp256k1(publicKey: PublicKey): Boolean =
    publicKey match {
      case p: ECPublicKey =>
        ParameterSpec(p.getParams) == Secp256k1
      case _ => false
    }

  def publicAddress(publicKey: PublicKey): Option[Array[Byte]] =
    publicKey match {
      case p: ECPublicKey if isSecp256k1(publicKey) =>
        val publicKey = Array.ofDim[Byte](64)
        val x         = p.getW.getAffineX.toByteArray.takeRight(32)
        val y         = p.getW.getAffineY.toByteArray.takeRight(32)
        x.copyToArray(publicKey, 32 - x.length)
        y.copyToArray(publicKey, 64 - y.length)
        Some(publicAddress(publicKey))
      case _ => None
    }

  def publicAddress(input: Array[Byte]): Array[Byte] =
    Keccak256.hash(input).drop(12)

  def from(certFilePath: String): X509Certificate =
    fromFile(new File(certFilePath))

  def fromFile(certFile: File): X509Certificate = {
    val cf = CertificateFactory.getInstance("X.509")
    val is = new FileInputStream(certFile)
    cf.generateCertificate(is).asInstanceOf[X509Certificate]
  }

  def generate(path: String): Unit = {
    import sys.process._
    Process(s"openssl ecparam -name secp256k1 -out $path/secp256k1.pem").!
    Process(
      s"openssl req -newkey ec:$path/secp256k1.pem -nodes " +
        s"-keyout $path/node.key.pem -x509 -days 365 " +
        s"-out $path/node.certificate.pem -subj /CN=local").!
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
