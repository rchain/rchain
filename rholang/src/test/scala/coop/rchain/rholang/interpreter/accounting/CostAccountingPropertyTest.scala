package coop.rchain.rholang.interpreter.accounting
import coop.rchain.rholang.{PrettyProc, ProcGen}
import coop.rchain.rholang.interpreter.Interpreter
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.Proc
import org.scalacheck.Arbitrary
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

class CostAccountingPropertyTest extends FlatSpec with PropertyChecks with Matchers {

  implicit val procArbitrary = Arbitrary(ProcGen.topLevelGen(5).map(PrettyProc(_)))

  def cost(proc: Proc): Cost = Cost(Interpreter.buildPar(proc).apply)

  behavior of "Cost accounting in Reducer"

  it should "cost of substitution is never negative" in {
    forAll { p: PrettyProc =>
      cost(p.proc).value should be >= 0L
    }

  }
}
