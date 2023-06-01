package coop.rchain.crypto.util

import java.io.FileWriter
import java.math.BigInteger
import java.nio.file.Path
import java.security.KeyFactory
import java.security.spec.ECPublicKeySpec
import cats.effect.{Resource, Sync}
import cats.syntax.all._
import coop.rchain.crypto.signatures.{Secp256k1, SignaturesAlg}
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.shared.Base16
import org.bouncycastle.jce.{ECNamedCurveTable, ECPointUtil}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.jce.spec.{ECNamedCurveSpec, ECPrivateKeySpec}
import org.bouncycastle.openssl.jcajce.{JcaMiscPEMGenerator, JcaPEMWriter, JcePEMEncryptorBuilder}
import org.bouncycastle.util.io.pem.PemObject

object KeyUtil {

  def writeKeys[F[_]: Sync](
      sk: PrivateKey,
      pk: PublicKey,
      sigAlgorithm: SignaturesAlg,
      password: String,
      privateKeyPemPath: Path,
      publicKeyPemPath: Path,
      publicKeyHexPath: Path
  ): F[Unit] = {
    // Equivalent of using
    // 1. `openssl ec -in key.pem -out privateKey.pem -aes256`
    // 2. `openssl ec -in privateKey.pem -pubout >> publicKey.pem`
    val encryptor =
      new JcePEMEncryptorBuilder("AES-256-CBC")
        .setSecureRandom(SecureRandomUtil.secureRandomNonBlocking)
        .setProvider(new BouncyCastleProvider())
        .build(password.toCharArray)
    for {
      keyPairs <- sigAlgorithm match {
                   case Secp256k1 =>
                     val s               = new BigInteger(Base16.encode(sk.bytes), 16)
                     val ecParameterSpec = ECNamedCurveTable.getParameterSpec(Secp256k1.name)
                     val params = new ECNamedCurveSpec(
                       Secp256k1.name,
                       ecParameterSpec.getCurve,
                       ecParameterSpec.getG,
                       ecParameterSpec.getN
                     )
                     val ecPoint        = ECPointUtil.decodePoint(params.getCurve, pk.bytes)
                     val privateKeySpec = new ECPrivateKeySpec(s, ecParameterSpec)
                     val pubKeySpec     = new ECPublicKeySpec(ecPoint, params)
                     val keyFactory     = KeyFactory.getInstance("ECDSA", new BouncyCastleProvider())
                     Sync[F].delay(
                       keyFactory.generatePrivate(privateKeySpec) ->
                         keyFactory.generatePublic(pubKeySpec)
                     )
                   case _ => Sync[F].raiseError(new Exception("Invalid algorithm"))
                 }
      (privateKey, publicKey) = keyPairs
      privatePemGenerator     = new JcaMiscPEMGenerator(privateKey, encryptor)
      publicPemGenerator      = new JcaMiscPEMGenerator(publicKey)
      priPemObj               = privatePemGenerator.generate()
      pubPemObj               = publicPemGenerator.generate()
      _                       <- writePem(privateKeyPemPath, priPemObj)
      _                       <- writePem(publicKeyPemPath, pubPemObj)
      _                       <- writeKey(publicKeyHexPath, Base16.encode(pk.bytes) + "\n")
    } yield ()
  }

  private def writePem[F[_]: Sync](path: Path, pemObject: PemObject) =
    Resource
      .make(
        Sync[F].delay(new JcaPEMWriter(new FileWriter(path.toFile)))
      )(writer => Sync[F].delay(writer.close()))
      .use { writer =>
        Sync[F].delay(writer.writeObject(pemObject))
      }

  private def writeKey[F[_]: Sync](path: Path, key: String) =
    Resource
      .make(
        Sync[F].delay(new FileWriter(path.toFile))
      )(writer => Sync[F].delay(writer.close()))
      .use { writer =>
        Sync[F].delay(writer.write(key))
      }
}
