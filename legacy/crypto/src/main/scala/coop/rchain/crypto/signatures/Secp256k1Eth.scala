package coop.rchain.crypto.signatures

import java.nio.file.Path

import cats.effect.Sync
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.crypto.util.CertificateHelper

/*
 * Ethereum personal signature
 *
 * The difference from `Secp256k1` is in signature format
 */
object Secp256k1Eth extends SignaturesAlg {

  val name                    = s"${Secp256k1.name}:eth"
  override val sigLength: Int = Secp256k1.sigLength

  def newKeyPair = Secp256k1.newKeyPair

  def parsePemFile[F[_]: Sync](path: Path, password: String): F[PrivateKey] =
    Secp256k1.parsePemFile(path, password)

  //TODO: refactor to make use of strongly typed keys
  /**
    * Verifies the given secp256k1 signature in native code.
    *
    * Input values
    * @param data The data which was signed, must be exactly 32 bytes
    * @param signatureRS The signature in raw RS format
    * @param pub The public key which did the signing
    *
    * Return value
    * boolean value of verification
    *
    */
  def verify(
      data: Array[Byte],
      signatureRS: Array[Byte],
      pub: Array[Byte]
  ): Boolean =
    // Convert signature to DER format
    CertificateHelper
      .encodeSignatureRStoDER(signatureRS)
      // DER conversion error silently returns false,
      // DERConverterSpec checks that this is only if input array is empty.
      .fold(_ => false, Secp256k1.verify(data, _, pub))

  /**
    * libsecp256k1 Create an ECDSA signature.
    *
    * Input values
    * @param data Message hash, 32 bytes
    * @param sec Secret key, 32 bytes
    *
    * Return value
    * byte array of signature in raw RS format
    *
    */
  def sign(
      data: Array[Byte],
      sec: Array[Byte]
  ): Array[Byte] = {
    val sigDER = Secp256k1.sign(data, sec)

    // Convert signature to raw RS format
    CertificateHelper
      .decodeSignatureDERtoRS(sigDER)
      // DER conversion error silently returns empty array,
      // DERConverterSpec checks that this is only if input array is empty.
      .getOrElse(Array[Byte]())
  }

  def secKeyVerify = Secp256k1.secKeyVerify(_)

  def toPublic(seckey: Array[Byte]): Array[Byte] =
    Secp256k1.toPublic(seckey)

  override def toPublic(sec: PrivateKey): PublicKey = Secp256k1.toPublic(sec)
}
