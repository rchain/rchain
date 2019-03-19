package coop.rchain.node.configuration.commandline
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Ed25519
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class Ed25519PubKeyConverterSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  property("parse works correctly for valid keys") {
    val gen = Gen.listOfN(Ed25519.publicKeyLength, Arbitrary.arbByte.arbitrary).map(_.toArray)

    forAll(gen) { bytes: Array[Byte] =>
      val asHex = Base16.encode(bytes)

      def getResult(s: String) =
        Ed25519PubKeyConverter.parse(List(("--user-id", List(s)))).right.get.get.bytes

      getResult(asHex) should be(bytes)
      getResult(asHex.toUpperCase) should be(bytes)
      getResult(asHex.toLowerCase) should be(bytes)
    }
  }

  property("parse returns error for bad key sizes") {
    val gen = Gen.listOfN(32, Arbitrary.arbByte.arbitrary).map(_.toArray)

    forAll(gen) { bytes: Array[Byte] =>
      val asHex = Base16.encode(bytes)
      val args  = List(("--user-id", List(asHex)))

      Ed25519PubKeyConverter.parse(args).isRight should be(bytes.length == Ed25519.publicKeyLength)
    }
  }

  property("parse returns error for bad characters") {

    forAll { s: String =>
      val args = List(("--user-id", List(s)))

      val isBadEncoding = s.isEmpty || s.exists(
        c => !(c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
      )

      Ed25519PubKeyConverter.parse(args).isLeft should be(isBadEncoding)
    }
  }
}
