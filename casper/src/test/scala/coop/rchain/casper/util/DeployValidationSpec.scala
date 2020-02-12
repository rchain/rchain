package coop.rchain.casper.util

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{DeployData, DeployDataProto}
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1, Secp256k1Eth, SignaturesAlg, Signed}
import org.scalatest.{FlatSpec, Matchers}

class DeployValidationSpec extends FlatSpec with Matchers {

  def createFromDeployDataProto(alg: SignaturesAlg) = {
    val deploy = DeployData(
      term = "Nil",
      timestamp = 111111,
      phloLimit = 1000000,
      phloPrice = 1,
      validAfterBlockNumber = 0L
    )

    // TODO: fixed key is used because Secp256k1Eth sign/verify is unstable with some generated keys ???
    // - check CertificateHelper.{encodeSignatureRStoDER, decodeSignatureDERtoRS}
    val privKeyHex = "6a42f5941bdaec35fdfa93d735c04f58d5d51ca3529d9a2fe753b818f1fa32e1"
    val privKey    = PrivateKey(Base16.unsafeDecode(privKeyHex))
//    val (privKey, _) = alg.newKeyPair

    val signed = Signed(deploy, alg, privKey)
    val deployProto = DeployDataProto()
      .withSigAlgorithm(alg.name)
      .withSig(signed.sig)
      .withDeployer(ByteString.copyFrom(signed.pk.bytes))
      .withTerm(signed.data.term)
      .withTimestamp(signed.data.timestamp)
      .withPhloPrice(signed.data.phloPrice)
      .withPhloLimit(signed.data.phloLimit)
      .withValidAfterBlockNumber(signed.data.validAfterBlockNumber)
    DeployData.from(deployProto)
  }

  "Secp256k1" should "be valid signature algorithm to sign a deploy" in {
    val validAlgs = Seq(Secp256k1, Secp256k1Eth)

    validAlgs.foreach { d =>
      createFromDeployDataProto(d) shouldBe a[Right[_, _]]
    }
  }

  "Ed25519" should "be invalid signature algorithm to sign a deploy" in {
    val inValidAlgs = Seq(Ed25519)

    inValidAlgs.foreach { d =>
      createFromDeployDataProto(d) shouldBe a[Left[_, _]]
    }
  }

}
