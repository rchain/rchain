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
    * {{{
    * >>> import coop.rchain.crypto.hash.Sha256
    * >>> import coop.rchain.crypto.{PrivateKey, PublicKey}
    * >>> val (PrivateKey(sec), PublicKey(pub)) = Secp256k1.newKeyPair
    * >>> val data = Sha256.hash("testing".getBytes)
    * >>> val sig = Secp256k1.sign(data, sec)
    * >>> Secp256k1.verify(data, sig, pub)
    * true
    * }}}
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
    * {{{
    * >>> import coop.rchain.crypto.hash._
    * >>> import coop.rchain.crypto.codec._
    * >>> val data = Sha256.hash("testing".getBytes)
    * >>> val sig = Base16.decode("3044022079BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F817980220294F14E883B3F525B5367756C2A11EF6CF84B730B36C17CB0C56F0AAB2C98589")
    * >>> val pub = Base16.decode("040A629506E1B65CD9D2E0BA9C75DF9C4FED0DB16DC9625ED14397F0AFC836FAE595DC53F8B0EFE61E703075BD9B143BAC75EC0E19F82A2208CAEB32BE53414C40")
    * >>> Secp256k1.verify(data, sig, pub)
    * true
    * }}}
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
    * {{{
    * >>> import coop.rchain.crypto.hash._
    * >>> import coop.rchain.crypto.codec._
    * >>> val data = Sha256.hash("testing".getBytes)
    * >>> val sec = Base16.decode("67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530")
    * >>> Base16.encode(Secp256k1.sign(data, sec)).toUpperCase()
    * 30440220182A108E1448DC8F1FB467D06A0F3BB8EA0533584CB954EF8DA112F1D60E39A202201C66F36DA211C087F3AF88B50EDF4F9BDAA6CF5FD6817E74DCA34DB12390C6E9
    * }}}
    *
    * Input values
    * @param data Message hash, 32 bytes
    * @param sec Secret key, 32 bytes
    *
    * Return value
    * byte array of signature
    *
  **/
  def sign(
      data: Array[Byte],
      sec: Array[Byte]
  ): Array[Byte] =
    NativeSecp256k1.sign(data, sec)

  /**
    * libsecp256k1 Seckey Verify - returns true if valid, false if invalid
    *
    * {{{
    * >>> import coop.rchain.crypto.codec._
    * >>> val sec = Base16.decode("67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530")
    * >>> Secp256k1.secKeyVerify(sec)
    * true
    * }}}
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
    * {{{
    * >>> import coop.rchain.crypto.codec._
    * >>> val sec = Base16.decode("67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530")
    * >>> Base16.encode(Secp256k1.toPublic(sec)).toUpperCase()
    * 04C591A8FF19AC9C4E4E5793673B83123437E975285E7B442F4EE2654DFFCA5E2D2103ED494718C697AC9AEBCFD19612E224DB46661011863ED2FC54E71861E2A6
    * }}}
    *
    * @param seckey ECDSA Secret key, 32 bytes
    *
    * Return values
    * @param pubkey ECDSA Public key, 33 or 65 bytes
    */
  def toPublic(seckey: Array[Byte]): Array[Byte] =
    NativeSecp256k1.computePubkey(seckey)

}
