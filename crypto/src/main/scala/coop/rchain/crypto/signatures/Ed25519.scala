package coop.rchain.crypto.signatures

import org.abstractj.kalium.keys._

object Ed25519 {

  //TODO: Make use of strongly typed keys
  def newKeyPair: (Array[Byte], Array[Byte]) = {
    val key = new SigningKey()
    val sec = key.toBytes()
    val pub = key.getVerifyKey().toBytes()
    (sec, pub)
  }

  /**
    * Ed25519 Compute Pubkey - computes public key from secret key
    *
    * @param seckey Ed25519 Secret key, 32 bytes
    *
    * Return values
    * @param pubkey Ed25519 Public key, 32 bytes
    */
  def toPublic(sec: Array[Byte]): Array[Byte] = {
    val key = new SigningKey(sec)
    key.getVerifyKey().toBytes()
  }

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
          throw ex
        }
    }

  /**
    * Create an Ed25519 signature.
    *
    * Input values
    * @param data Message hash, 32 bytes
    * @param sec Secret key, 32 bytes
    *
    * Return value
    * byte array of signature
    *
    */
  def sign(
      data: Array[Byte],
      sec: Array[Byte]
  ): Array[Byte] =
    new SigningKey(sec).sign(data)

}
