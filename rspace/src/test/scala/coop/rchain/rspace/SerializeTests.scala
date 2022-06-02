package coop.rchain.rspace

import coop.rchain.rspace.examples.StringExamples.Pattern
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.test.ArbitraryInstances._
import coop.rchain.rspace.test.roundTripCodec
import org.scalacheck.Prop
import org.scalactic.anyvals.PosInt
import org.scalatest.prop.Configuration
import org.scalatestplus.scalacheck.Checkers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scodec.DecodeResult

class SerializeTests extends AnyFlatSpec with Matchers with Checkers with Configuration {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = PosInt(1000))

  "A String" should "round trip through a derived codec" in {

    val propRoundTripCodec: Prop = Prop.forAll { (str: String) =>
      roundTripCodec(str)(stringSerialize.toSizeHeadCodec)
        .map((value: DecodeResult[String]) => value.value == str)
        .getOrElse(default = false)
    }

    check(propRoundTripCodec)
  }

  "A Pattern" should "round trip through a derived codec" in {

    val propRoundTripCodec: Prop = Prop.forAll { (pat: Pattern) =>
      roundTripCodec(pat)(patternSerialize.toSizeHeadCodec)
        .map((value: DecodeResult[Pattern]) => value.value == pat)
        .getOrElse(default = false)
    }

    check(propRoundTripCodec)
  }
}
