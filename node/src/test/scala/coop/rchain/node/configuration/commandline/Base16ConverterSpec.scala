package coop.rchain.node.configuration.commandline
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class Base16ConverterSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  property("parse returns error for bad characters") {

    forAll { s: String =>
      val args = List(("", List(s)))

      val hasInvalidCharacters = s.replaceAll("[^0-9A-Fa-f]", "") != s

      Base16Converter.parse(args).isLeft should be(hasInvalidCharacters)
    }
  }
}
