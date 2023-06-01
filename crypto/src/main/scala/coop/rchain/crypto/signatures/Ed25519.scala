package coop.rchain.crypto.signatures

import coop.rchain.crypto
import coop.rchain.crypto.{PrivateKey, PublicKey}
import org.abstractj.kalium.keys._

import java.util.Locale

object Ed25519 extends SignaturesAlg {

  val keyLength          = 32
  override val sigLength = 64

  val name: String = "Ed25519".toLowerCase(Locale.getDefault)

  //TODO: Make use of strongly typed keys
  def newKeyPair: (PrivateKey, PublicKey) = {
    val key = new SigningKey()
    val sec = key.toBytes
    val pub = key.getVerifyKey.toBytes
    (PrivateKey(sec), PublicKey(pub))
  }

  /**
    * Ed25519 Compute Pubkey - computes public key from secret key
    *
    * @param sec Ed25519 Secret key, 32 bytes
    *
    * @return Ed25519 Public key, 32 bytes
    */
  def toPublic(sec: Array[Byte]): Array[Byte] = {
    val key = new SigningKey(sec)
    key.getVerifyKey.toBytes
  }

  override def toPublic(sec: crypto.PrivateKey): crypto.PublicKey =
    crypto.PublicKey(toPublic(sec.bytes))

  /**
    * Verifies the given Ed25519 signature.
    *
    * Input values
    * @param data The data which was signed, must be exactly 32 bytes
    * @param signature The signature
    * @param pub The public key which did the signing
    *
    * Return value
    * boolean value of verification
    *
    */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def verify(
      data: Array[Byte],
      signature: Array[Byte],
      pub: Array[Byte]
  ): Boolean =
    try {
      new VerifyKey(pub).verify(data, signature)
    } catch {
      case ex: RuntimeException =>
        if (ex.getMessage contains "signature was forged or corrupted") {
          false
        } else {
          throw ex // should we return false?
        }
    }

  /**
    * Create an Ed25519 signature.
    *
    * Input values
    * @param data Message hash, 32 bytes
    * @param sec Secret key, 32 bytes
    *
    * @return byte array of signature
    *
    */
  def sign(
      data: Array[Byte],
      sec: Array[Byte]
  ): Array[Byte] =
    new SigningKey(sec).sign(data)
}
