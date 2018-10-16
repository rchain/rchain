package coop.rchain.rholang.interpreter.accounting
import coop.rchain.rholang.ProcGen
import coop.rchain.rholang.interpreter.Interpreter
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.Proc
import coop.rchain.rholang.syntax.rholang_mercury.PrettyPrinter
import org.scalacheck.{Arbitrary, Shrink}
import org.scalacheck.Test.Parameters
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

case class PrettyProc(proc: Proc) {
  override def toString = PrettyPrinter.print(proc)
}

class CostAccountingPropertyTest extends FlatSpec with PropertyChecks with Matchers {
  import ProcGen.procShrinker

  implicit val params: Parameters = Parameters.defaultVerbose.withMinSuccessfulTests(1000)
  implicit val procArbitrary      = Arbitrary(ProcGen.topLevelGen(5).map(PrettyProc(_)))
  implicit def shrinkProc: Shrink[PrettyProc] = Shrink { pp =>
    Shrink.shrink(pp.proc).map(PrettyProc(_))
  }

  def cost(proc: Proc): Cost = Cost(Interpreter.buildPar(proc).apply)

  behavior of "Cost accounting in Reducer"

  it should "cost of substitution is never negative" in {
    forAll { p: PrettyProc =>
      cost(p.proc).value should be >= 0L
    }

  }
}
