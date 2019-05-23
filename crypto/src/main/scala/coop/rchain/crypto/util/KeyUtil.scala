package coop.rchain.crypto.util

import java.io.FileWriter
import java.math.BigInteger
import java.nio.file.Path
import java.security.KeyFactory

import cats.effect.Resource
import cats.implicits._
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1, SignaturesAlg}
import monix.eval.Task
import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.jce.ECNamedCurveTable
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.jce.spec.{ECParameterSpec, ECPrivateKeySpec}
import org.bouncycastle.openssl.jcajce.{JcaMiscPEMGenerator, JcaPEMWriter, JcePEMEncryptorBuilder}

object KeyUtil {

  def writePrivateKey(
      sk: PrivateKey,
      sigAlgorithm: SignaturesAlg,
      password: String,
      privateKeyPath: Path
  ): Task[Unit] = {
    // Equivalent of using `openssl ec -in key.pem -out key_enc.pem -aes256`
    val encryptor =
      new JcePEMEncryptorBuilder("AES-256-CBC")
        .setSecureRandom(SecureRandomUtil.secureRandomNonBlocking)
        .setProvider(new BouncyCastleProvider())
        .build(password.toCharArray)
    for {
      privateKey <- sigAlgorithm match {
                     case Secp256k1 =>
                       val s               = new BigInteger(Base16.encode(sk.bytes), 16)
                       val ecParameterSpec = ECNamedCurveTable.getParameterSpec(Secp256k1.name)
                       val privateKeySpec  = new ECPrivateKeySpec(s, ecParameterSpec)
                       val keyFactory      = KeyFactory.getInstance("ECDSA", new BouncyCastleProvider())
                       keyFactory.generatePrivate(privateKeySpec).pure[Task]
                     case Ed25519 =>
                       val s           = new BigInteger(Base16.encode(sk.bytes), 16)
                       val ecParameter = CustomNamedCurves.getByName("Curve25519")
                       val ecParameterSpec = new ECParameterSpec(
                         ecParameter.getCurve,
                         ecParameter.getG,
                         ecParameter.getN,
                         ecParameter.getH,
                         ecParameter.getSeed
                       )
                       val privateKeySpec = new ECPrivateKeySpec(s, ecParameterSpec)
                       val keyFactory     = KeyFactory.getInstance("ECDH", new BouncyCastleProvider())
                       keyFactory.generatePrivate(privateKeySpec).pure[Task]
                     case _ => Task.raiseError(new Exception("Invalid algorithm"))
                   }
      pemGenerator = new JcaMiscPEMGenerator(privateKey, encryptor)
      pemObj       = pemGenerator.generate()
      _ <- Resource
            .make(
              Task.delay(new JcaPEMWriter(new FileWriter(privateKeyPath.toFile)))
            )(writer => Task.delay(writer.close()))
            .use { writer =>
              Task.delay(writer.writeObject(pemObj))
            }
    } yield ()
  }
}
