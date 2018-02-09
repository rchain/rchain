package coop.rchain.crypto.signatures

import org.bitcoin._

object Secp256k1 {

  /**
  * Verifies the given secp256k1 signature in native code.
  * Calling when enabled == false is undefined (probably library not loaded)
  *
  * @param data The data which was signed, must be exactly 32 bytes
  * @param signature The signature
  * @param pub The public key which did the signing
  */
  def verify(
    data: Array[Byte], signature: Array[Byte], pub: Array[Byte]
    ): Boolean =
    NativeSecp256k1.verify(data,signature,pub)

  /**
  * libsecp256k1 Create an ECDSA signature.
  *
  * @param data Message hash, 32 bytes
  * @param sec Secret key, 32 bytes
  *
  * Return values
  * @param sig byte array of signature
  **/
  def sign(
    data: Array[Byte], sec: Array[Byte]
    ): Array[Byte] =
    NativeSecp256k1.sign(data,sec)

  /**
  * libsecp256k1 Seckey Verify - returns true if valid, false if invalid
  *
  * @param seckey ECDSA Secret key, 32 bytes
  */
  def secKeyVerify(seckey: Array[Byte]): Boolean =
    NativeSecp256k1.secKeyVerify(seckey)

  /**
  * libsecp256k1 Compute Pubkey - computes public key from secret key
  *
  * @param seckey ECDSA Secret key, 32 bytes
  *
  * Return values
  * @param pubkey ECDSA Public key, 33 or 65 bytes
  */
  def computePubkey(seckey: Array[Byte]): Array[Byte] =
    computePubkey(seckey)

}