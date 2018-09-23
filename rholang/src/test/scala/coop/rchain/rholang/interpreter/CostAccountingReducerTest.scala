package coop.rchain.rholang.interpreter

import coop.rchain.models.Expr.ExprInstance.{EVarBody, GString}
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar}
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.accounting.{Chargeable, Cost, CostAccount, CostAccountingAlg}
import monix.eval.Coeval
import org.scalactic.TripleEqualsSupport
import org.scalatest.{FlatSpec, Matchers}

class CostAccountingReducerTest extends FlatSpec with Matchers with TripleEqualsSupport {

  behavior of "Cost accounting in Reducer"

  it should "charge for the successful substitution" in {
    val term: Expr => Par = expr => Par(bundles = Seq(Bundle(Par(exprs = Seq(expr)))))
    val varTerm           = term(Expr(EVarBody(EVar(Var(BoundVar(0))))))
    val substTerm         = term(Expr(GString("1")))
    val env               = Env.makeEnv[Par](Expr(GString("1")))
    val termCost          = Chargeable[Par].cost(substTerm)
    val initCost          = CostAccount(1000)
    val costAlg           = CostAccountingAlg.unsafe[Coeval](initCost)
    val res               = Reduce.substituteAndCharge[Par, Coeval](varTerm, 0, env, costAlg).attempt.value
    assert(res === Right(substTerm))
    assert(costAlg.get().value.cost === (initCost.cost - Cost(termCost)))
  }

  it should "charge for failed substitution" in {
    val term: Expr => Par = expr => Par(bundles = Seq(Bundle(Par(exprs = Seq(expr)))))
    val varTerm           = term(Expr(EVarBody(EVar(Var(FreeVar(0))))))
    val env               = Env.makeEnv[Par](Expr(GString("1")))
    val originalTermCost  = Chargeable[Par].cost(varTerm)
    val initCost          = CostAccount(1000)
    val costAlg           = CostAccountingAlg.unsafe[Coeval](initCost)
    val res               = Reduce.substituteAndCharge[Par, Coeval](varTerm, 0, env, costAlg).attempt.value
    assert(res.isLeft)
    assert(costAlg.get().value.cost === (initCost.cost - Cost(originalTermCost)))
  }

  it should "stop if OutOfPhloError is returned from RSpace" in {
    pending
  }

  it should "update the cost account after going back from RSpace" in {
    pending
  }

  it should "stop interpreter threads as soon as deploy runs out of phlo" in {
    pending
  }
}
