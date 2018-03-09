package coop.rchain.models

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import testImplicits._

class PrettyPrinterTest extends FlatSpec with PropertyChecks with Matchers {

  // This test doesn't actually check anything.
  // It is for checking the pretty print of an arbitrarily generated Par.
  "Par" should "Pretty print " in {
    forAll { (par: Par) =>
      PrettyPrinter.prettyPrint(par)
      println()
    }
  }
}
