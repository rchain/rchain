package coop.rchain.rholang

import coop.rchain.rholang.interpreter.Interpreter
import org.scalacheck.Arbitrary
import org.scalacheck.Test.Parameters
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

class ProcGenTest extends FlatSpec with PropertyChecks with Matchers {
  implicit val params: Parameters = Parameters.defaultVerbose.withMinSuccessfulTests(1000)

  implicit val procArbitrary = Arbitrary(ProcGen.topLevelGen(5).map(PrettyProc(_)))

  behavior of "ProcGen"

  it should "all the procs generated are normalized successfully" in {
    forAll { p: PrettyProc =>
      Interpreter.buildPar(p.proc).apply
    }
  }

}
