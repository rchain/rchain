package coop.rchain.crypto.util

import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.{Secp256k1, Secp256k1Eth}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class DERConverterSpec extends PropSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  implicit val arbBytes: Arbitrary[Array[Byte]] = arbContainer[Array, Byte]

  property("DER converter check with valid and non-empty input") {
    forAll { (bytes: Array[Byte]) =>
      if (!bytes.isEmpty) {
        val (PrivateKey(privKey), _) = Secp256k1.newKeyPair
        val data                     = Blake2b256.hash(bytes)
        val sigRS                    = Secp256k1Eth.sign(data, privKey)

        val sigDER     = CertificateHelper.encodeSignatureRStoDER(sigRS).get
        val expectedRS = CertificateHelper.decodeSignatureDERtoRS(sigDER).get

        // Encode / decode should get initial input for valid signature
        sigRS shouldBe expectedRS

        // Encoder is safe of exception for any input
        CertificateHelper.encodeSignatureRStoDER(bytes).get.isEmpty shouldBe false

        // Decoder should throw exception with invalid DER message format
        CertificateHelper
          .decodeSignatureDERtoRS(bytes)
          .failed
          .get shouldBe a[IllegalArgumentException]
      }
    }
  }

  property("encoder should throw exception on empty input") {
    CertificateHelper
      .encodeSignatureRStoDER(Array[Byte]())
      .failed
      .get shouldBe a[IllegalArgumentException]
  }

  property("decoder should throw exception on empty input") {
    CertificateHelper
      .decodeSignatureDERtoRS(Array[Byte]())
      .failed
      .get shouldBe a[IllegalArgumentException]
  }

}
