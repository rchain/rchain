package coop.rchain.crypto.signatures

import java.security.KeyPairGenerator
import java.security.interfaces.ECPrivateKey
import java.security.spec.ECGenParameterSpec

import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.util.SecureRandomUtil
import coop.rchain.crypto.{PrivateKey, PublicKey}
import org.bitcoin._
import com.google.common.base.Strings
import org.bouncycastle.jce.provider.BouncyCastleProvider

object Secp256k1 {

  private val provider  = new BouncyCastleProvider()
  private val curveName = "secp256k1"

  /**
    * Verifies the given secp256k1 signature in native code.
    *
    * @return (private key, public key) pair
    *
    */
  def newKeyPair: (PrivateKey, PublicKey) = {
    val kpg = KeyPairGenerator.getInstance("ECDSA", provider)
    kpg.initialize(new ECGenParameterSpec(curveName), SecureRandomUtil.secureRandomNonBlocking)
    val kp = kpg.generateKeyPair

    val padded =
      Strings.padStart(kp.getPrivate.asInstanceOf[ECPrivateKey].getS.toString(16), 64, '0')
    val sec = Base16.decode(padded)
    val pub = Secp256k1.toPublic(sec)

    (PrivateKey(sec), PublicKey(pub))
  }

  //TODO: refactor to make use of strongly typed keys
  /**
    * Verifies the given secp256k1 signature in native code.
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
    NativeSecp256k1.verify(data, signature, pub)

  /**
    * libsecp256k1 Create an ECDSA signature.
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
    NativeSecp256k1.sign(data, sec)

  /**
    * libsecp256k1 Seckey Verify - returns true if valid, false if invalid
    *
    * Input value
    * @param seckey ECDSA Secret key, 32 bytes
    *
    * Return value
    * Boolean of secret key verification
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
