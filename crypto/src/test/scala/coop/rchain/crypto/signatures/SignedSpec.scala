package coop.rchain.crypto.signatures

import com.google.protobuf.ByteString
import coop.rchain.crypto.signatures.Signed.{signatureHash}
import coop.rchain.shared.Serialize
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector

class SignedSpec extends PropSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  implicit val serializable: Serialize[Array[Byte]] = new Serialize[Array[Byte]] {
    override def encode(bytes: Array[Byte]): ByteVector = ByteVector(bytes)

    override def decode(bytes: ByteVector): Either[Throwable, Array[Byte]] = Right(bytes.toArray)
  }

  implicit val sigAlgorithmArbitrary: Arbitrary[SignaturesAlg] = Arbitrary(
    Gen.oneOf(Secp256k1, Secp256k1Eth, Ed25519)
  )

  property("Signed should generate a valid signature") {
    forAll { (sigAlgorithm: SignaturesAlg, input: Array[Byte]) =>
      val (sk, pk) = sigAlgorithm.newKeyPair

      val signed = Signed(input, sigAlgorithm, sk)
      val hash   = signatureHash(sigAlgorithm.name, signed.data)

      sigAlgorithm.verify(hash, signed.sig.toByteArray, pk) should be(true)
      signed.pk should be(pk)
      signed.sigAlgorithm should be(sigAlgorithm)
      signed.data should be(input)
    }
  }

  property("Signed.fromSignedData should recreate the Signed instance") {
    forAll { (sigAlgorithm: SignaturesAlg, input: Array[Byte]) =>
      val (sk, _) = sigAlgorithm.newKeyPair

      val signed = Signed(input, sigAlgorithm, sk)

      val fromSigned =
        Signed.fromSignedData(signed.data, signed.pk, signed.sig, signed.sigAlgorithm)

      fromSigned.isDefined should be(true)
      signed should be(fromSigned.get)
    }
  }

  property("Signed.fromSignedData should return null for invalid signatures") {
    forAll { (sigAlgorithm: SignaturesAlg, input: Array[Byte], randomByte: Byte) =>
      val (_, pk)    = sigAlgorithm.newKeyPair
      val invalidSig = Array.fill(sigAlgorithm.sigLength)(randomByte)

      val fromSigned =
        Signed.fromSignedData(input, pk, ByteString.copyFrom(invalidSig), sigAlgorithm)

      fromSigned should be(None)
    }
  }
}
