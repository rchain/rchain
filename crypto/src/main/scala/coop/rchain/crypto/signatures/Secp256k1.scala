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
  * {{{
  * >>> import coop.rchain.crypto.hash._
  * >>> import coop.rchain.crypto.codec._
  * >>> val data = Sha256.hash("testing".getBytes)
  * >>> val sig = Base16.decode("3044022079BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F817980220294F14E883B3F525B5367756C2A11EF6CF84B730B36C17CB0C56F0AAB2C98589")
  * >>> val pub = Base16.decode("040A629506E1B65CD9D2E0BA9C75DF9C4FED0DB16DC9625ED14397F0AFC836FAE595DC53F8B0EFE61E703075BD9B143BAC75EC0E19F82A2208CAEB32BE53414C40")
  * >>> Secp256k1.verify(data, sig, pub)
  * true
  * }}}
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
  def toPublic(seckey: Array[Byte]): Array[Byte] =
    NativeSecp256k1.computePubkey(seckey)

}