package coop.rchain.crypto.util

import java.io.{File, FileInputStream}
import java.math.BigInteger
import java.security._
import java.security.cert._
import java.security.interfaces.{ECPrivateKey, ECPublicKey}
import java.security.spec._
import java.util.Base64

import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Keccak256

import scala.io.Source

object CertificateHelper {

  val EllipticCurveName = "secp256r1"

  lazy val EllipticCurveParameterSpec: ParameterSpec = {
    val ap = AlgorithmParameters.getInstance("EC")
    ap.init(new ECGenParameterSpec(EllipticCurveName))
    ParameterSpec(ap.getParameterSpec(classOf[ECParameterSpec]))
  }

  def isExpectedEllipticCurve(publicKey: PublicKey): Boolean =
    publicKey match {
      case p: ECPublicKey =>
        ParameterSpec(p.getParams) == EllipticCurveParameterSpec
      case _ => false
    }

  def publicAddress(publicKey: PublicKey): Option[Array[Byte]] =
    publicKey match {
      case p: ECPublicKey if isExpectedEllipticCurve(publicKey) =>
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

  def readKeyPair(keyFile: File): KeyPair = {
    import coop.rchain.shared.Resources._
    val str = withResource(Source.fromFile(keyFile)) {
      _.getLines().filter(!_.contains("KEY")).mkString
    }
    val spec     = new PKCS8EncodedKeySpec(Base64.getDecoder.decode(str))
    val kf       = KeyFactory.getInstance("EC")
    val sk       = kf.generatePrivate(spec).asInstanceOf[ECPrivateKey]
    val ecSpec   = org.bouncycastle.jce.ECNamedCurveTable.getParameterSpec(EllipticCurveName)
    val Q        = ecSpec.getG.multiply(sk.getS).normalize()
    val pubPoint = new ECPoint(Q.getAffineXCoord.toBigInteger, Q.getAffineYCoord.toBigInteger)
    val pubSpec  = new ECPublicKeySpec(pubPoint, sk.getParams)
    val pk       = kf.generatePublic(pubSpec)
    new KeyPair(pk, sk)
  }

  def generateKeyPair(useNonBlockingRandom: Boolean): KeyPair = {
    val secureRandom =
      if (useNonBlockingRandom) SecureRandomUtil.secureRandomNonBlocking
      else new SecureRandom()
    val kpg = KeyPairGenerator.getInstance("EC")
    kpg.initialize(new ECGenParameterSpec(EllipticCurveName), secureRandom)
    kpg.generateKeyPair
  }

  def generate(keyPair: KeyPair): X509Certificate = {
    import sun.security.x509._

    val privateKey  = keyPair.getPrivate
    val publicKey   = keyPair.getPublic
    val address     = publicAddress(publicKey).map(Base16.encode).getOrElse("local")
    val algorythm   = "SHA256withECDSA"
    val algorithmId = new AlgorithmId(AlgorithmId.sha256WithECDSA_oid)

    val info     = new X509CertInfo
    val from     = new java.util.Date()
    val to       = new java.util.Date(from.getTime + 365 * 86400000L)
    val interval = new CertificateValidity(from, to)
    val serial   = new BigInteger(64, new SecureRandom())
    val owner    = new X500Name(s"CN=$address")

    info.set(X509CertInfo.VALIDITY, interval)
    info.set(X509CertInfo.SERIAL_NUMBER, new CertificateSerialNumber(serial))
    info.set(X509CertInfo.SUBJECT, owner)
    info.set(X509CertInfo.ISSUER, owner)
    info.set(X509CertInfo.KEY, new CertificateX509Key(publicKey))
    info.set(X509CertInfo.VERSION, new CertificateVersion(CertificateVersion.V3))
    info.set(X509CertInfo.ALGORITHM_ID, new CertificateAlgorithmId(algorithmId))

    // Sign the cert to identify the algorithm that's used.
    var cert = new X509CertImpl(info)
    cert.sign(privateKey, algorythm)

    // Update the algorith, and resign.
    val algorithmId2 = cert.get(X509CertImpl.SIG_ALG).asInstanceOf[AlgorithmId]
    info.set(CertificateAlgorithmId.NAME + "." + CertificateAlgorithmId.ALGORITHM, algorithmId2)
    cert = new X509CertImpl(info)
    cert.sign(privateKey, algorythm)
    cert
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
    ParameterSpec(
      ecParamSpec.getCurve,
      ecParamSpec.getGenerator,
      ecParamSpec.getOrder,
      ecParamSpec.getCofactor
    )
}

object CertificatePrinter {
  import scala.annotation.tailrec

  def print(certificate: X509Certificate): String = {
    val str = Base64.getEncoder.encodeToString(certificate.getEncoded)
    split(str).mkString("-----BEGIN CERTIFICATE-----\n", "\n", "\n-----END CERTIFICATE-----")
  }

  def printPrivateKey(privateKey: PrivateKey): String = {
    val str = Base64.getEncoder.encodeToString(privateKey.getEncoded)
    split(str).mkString("-----BEGIN PRIVATE KEY-----\n", "\n", "\n-----END PRIVATE KEY-----")
  }

  @tailrec
  private def split(s: String, acc: List[String] = Nil): List[String] =
    if (s.length == 0) acc.reverse
    else {
      val (a, b) = s.splitAt(64)
      split(b, a :: acc)
    }

}
