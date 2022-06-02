package coop.rchain.crypto.codec

import coop.rchain.shared.Base16
import org.scalacheck.Shrink
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class Base16Spec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  property("decode after encode returns the original input") {
    forAll { (input: Array[Byte]) =>
      val encoded = Base16.encode(input)
      val decoded = Base16.decode(encoded).get
      decoded should equal(input)
    }
  }

  property("decode fails if given non-hex characters") {
    forAll { (input: String) =>
      val decoded              = Base16.decode(input, "")
      val hasInvalidCharacters = input.replaceAll("[^0-9A-Fa-f]", "") != input
      decoded.isEmpty should equal(hasInvalidCharacters)
    }
  }

  property("decode fails if given ｃ (non-hex character)") {
    Base16.decode("ｃ") shouldBe empty
  }

  property("unsafeDecode is always successful") {
    forAll { (input: String) =>
      try {
        Base16.unsafeDecode(input)
      } catch {
        case _: Exception => Base16.unsafeDecode(input)
      }
    }
  }
}
