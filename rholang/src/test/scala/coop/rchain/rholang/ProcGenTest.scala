package coop.rchain.rholang

import coop.rchain.rholang.interpreter.Interpreter
import coop.rchain.rholang.syntax.rholang_mercury.Absyn._
import coop.rchain.rholang.syntax.rholang_mercury.PrettyPrinter
import org.scalacheck.Arbitrary
import org.scalacheck.Test.Parameters
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

class ProcGenTest extends FlatSpec with PropertyChecks with Matchers {
  implicit val params: Parameters = Parameters.defaultVerbose.withMinSuccessfulTests(1000)
  implicit val procArbitrary = Arbitrary(
    ProcGen.topLevelGen(5).map(PrettyPrinted[Proc](_, PrettyPrinter.print))
  )
  behavior of "Proc generator"

  it should "generate correct procs that are normalized successfully" in {

    forAll { p: PrettyPrinted[Proc] =>
      Interpreter.buildPar(p.value).apply
    }
  }

  behavior of "Proc shrinker"

  it should "produce a correct Proc" in {
    forAll { original: PrettyPrinted[Proc] =>
      ProcGen.procShrinker
        .shrink(original.value)
        .headOption
        .map(shrinked => Interpreter.buildPar(shrinked).apply)
        .map(Function.const(true))
        .getOrElse(true)

    }
  }

  it should "produce a shorter Proc when pretty printed" in {
    def length(p: Proc) = PrettyPrinter.print(p).length

    forAll { original: PrettyPrinted[Proc] =>
      ProcGen.procShrinker
        .shrink(original.value)
        .headOption
        .map(shrinked => length(original.value) >= length(shrinked))
        .getOrElse(true)
    }
  }
}
