package coop.rchain.rholang.interpreter.util.codec

import java.math.BigInteger

import org.scalacheck.{Gen, Shrink}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class Base58Properties extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  implicit val propertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 10000, sizeRange = 200)
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  private val genBase58Digit  = Gen.oneOf(Base58.alphabet)
  private def genBase58String = Gen.listOf(Gen.oneOf(Base58.alphabet)).map(_.mkString)
  private def genNonemptyBase58String =
    Gen.nonEmptyListOf(Gen.oneOf(Base58.alphabet)).map(_.mkString)

  property("decode after encode returns the original bytes") {
    forAll { (input: Array[Byte]) =>
      val encoded = Base58.encode(input)
      val decoded = Base58.decode(encoded).get
      decoded should equal(input)
    }
  }

  property("encode a after decode returns the original string") {
    forAll(genBase58String) { (input: String) =>
      val decoded = Base58.decode(input).get
      val encoded = Base58.encode(decoded)
      encoded should be(input)
    }
  }

  property("decode a string should fail if it contains invalid Base58 chars") {
    forAll { (input: String) =>
      val result = input.forall(c => Base58.alphabet.contains(c))
      Base58.decode(input).isDefined should be(result)
    }
  }

  property(
    "when changing a digit the result value changes in the same direction as the changed digit"
  ) {
    val generator =
      for {
        input        <- genNonemptyBase58String
        changedIndex <- Gen.choose(0, input.length - 1)
        newChar      <- genBase58Digit
      } yield (input, changedIndex, newChar)

    def toPositiveBigInt(bytes: Array[Byte]) = BigInt(new BigInteger(1, bytes))

    forAll(generator) {
      case (input: String, changedIndex, newChar) =>
        val originalDecoded = toPositiveBigInt(Base58.decode(input).get)

        val originalChar = input(changedIndex)

        val newInput =
          input.substring(0, changedIndex) +
            newChar +
            input.substring(changedIndex + 1)

        val newDecoded = toPositiveBigInt(Base58.decode(newInput).get)

        val newcharVariance      = originalChar < newChar
        val decodedValueVariance = originalDecoded < newDecoded

        decodedValueVariance should be(newcharVariance)
    }
  }
}
