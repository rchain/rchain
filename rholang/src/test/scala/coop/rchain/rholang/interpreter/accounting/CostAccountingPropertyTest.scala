package coop.rchain.rholang.interpreter.accounting
import coop.rchain.rholang.{PrettyPrinted, ProcGen}
import coop.rchain.rholang.interpreter.Interpreter
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.Proc
import coop.rchain.rholang.syntax.rholang_mercury.PrettyPrinter
import org.scalacheck.Arbitrary
import org.scalacheck.Test.Parameters
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

class CostAccountingPropertyTest extends FlatSpec with PropertyChecks with Matchers {
  implicit val params: Parameters = Parameters.defaultVerbose.withMinSuccessfulTests(1000)
  implicit val procArbitrary = Arbitrary(
    ProcGen.topLevelGen(5).map(PrettyPrinted[Proc](_, PrettyPrinter.print))
  )

  def cost(proc: Proc): Cost = Cost(Interpreter.buildPar(proc).apply)

  behavior of "Cost accounting in Reducer"

  it should "cost of substitution is never negative" in {
    forAll { p: PrettyPrinted[Proc] =>
      cost(p.value).value should be >= 0L
    }

  }
}
