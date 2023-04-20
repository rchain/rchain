package coop.rchain.crypto.signatures

import java.io.FileReader
import java.nio.file.Path
import java.security.KeyPairGenerator
import java.security.interfaces.ECPrivateKey
import java.security.spec.ECGenParameterSpec
import cats.effect.Sync
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.applicativeError._
import coop.rchain.crypto.util.SecureRandomUtil
import coop.rchain.crypto.{PrivateKey, PublicKey}
import org.bitcoin._
import com.google.common.base.Strings
import coop.rchain.shared.Base16
import org.bouncycastle.asn1.DLSequence
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.openssl.bc.BcPEMDecryptorProvider
import org.bouncycastle.openssl.{PEMEncryptedKeyPair, PEMParser}

// TODO: refactor Signature API to handle exceptions from `NativeSecp256k1` library

object Secp256k1 extends SignaturesAlg {

  private val provider        = new BouncyCastleProvider()
  val name                    = "secp256k1"
  override val sigLength: Int = 32

  /**
    * Verifies the given secp256k1 signature in native code.
    *
    * @return (private key, public key) pair
    *
    */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def newKeyPair: (PrivateKey, PublicKey) = {
    val kpg = KeyPairGenerator.getInstance("ECDSA", provider)
    kpg.initialize(new ECGenParameterSpec(name), SecureRandomUtil.secureRandomNonBlocking)
    val kp = kpg.generateKeyPair

    val padded =
      Strings.padStart(kp.getPrivate.asInstanceOf[ECPrivateKey].getS.toString(16), 64, '0')
    val sec = Base16.unsafeDecode(padded)
    val pub = Secp256k1.toPublic(sec)

    (PrivateKey(sec), PublicKey(pub))
  }

  def parsePemFile[F[_]: Sync](path: Path, password: String): F[PrivateKey] = {
    import scala.jdk.CollectionConverters._

    def handleWith[A](f: => A, message: String): F[A] =
      Sync[F].delay(f).recoverWith { case t => Sync[F].raiseError(new Exception(message, t)) }

    val parser = new PEMParser(new FileReader(path.toFile))
    for {
      encryptedKeyPair <- handleWith(
                           parser.readObject().asInstanceOf[PEMEncryptedKeyPair],
                           "PEM file is not encrypted"
                         )
      decryptor = new BcPEMDecryptorProvider(password.toCharArray)
      keyPair <- handleWith(
                  encryptedKeyPair.decryptKeyPair(decryptor),
                  "Could not decrypt PEM file"
                )
      dlSequence <- handleWith(
                     keyPair.getPrivateKeyInfo.parsePrivateKey.asInstanceOf[DLSequence],
                     "Could not parse private key from PEM file"
                   )
      privateKeyOpt = dlSequence.iterator().asScala.collectFirst {
        case octet: org.bouncycastle.asn1.DEROctetString => PrivateKey(octet.getOctets)
      }
      privateKey <- privateKeyOpt match {
                     case Some(privateKey) => privateKey.pure[F]
                     case None =>
                       Sync[F].raiseError[PrivateKey](
                         new Exception("PEM file does not contain private key")
                       )
                   }
    } yield privateKey
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
    // WARNING: this code throws Assertion exception if input is not correct length
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
    // WARNING: this code throws Assertion exception if input is not correct length
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
    // WARNING: this code throws Assertion exception if input is not correct length
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
    // WARNING: this code throws Assertion exception if input is not correct length
    NativeSecp256k1.computePubkey(seckey)

  override def toPublic(sec: PrivateKey): PublicKey = PublicKey(toPublic(sec.bytes))
}
