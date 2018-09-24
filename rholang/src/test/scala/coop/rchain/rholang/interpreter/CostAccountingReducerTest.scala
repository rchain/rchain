package coop.rchain.rholang.interpreter

import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Channel.ChannelInstance
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.Expr.ExprInstance.{EVarBody, GString}
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar}
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Reduce.DebruijnInterpreter
import coop.rchain.rholang.interpreter.accounting.{Chargeable, Cost, CostAccount, CostAccountingAlg}
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.storage.TuplespaceAlg
import monix.eval.{Coeval, Task}
import org.scalactic.TripleEqualsSupport
import org.scalatest.{FlatSpec, Matchers}
import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global

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
    val tuplespaceAlg = new TuplespaceAlg[Task] {
      override def produce(
          chan: Channel,
          data: ListChannelWithRandom,
          persistent: Boolean
      ): Task[Unit] =
        Task.raiseError(OutOfPhlogistonsError)
      override def consume(
          binds: Seq[(BindPattern, ChannelInstance.Quote)],
          body: ParWithRandom,
          persistent: Boolean
      ): Task[Unit] = Task.raiseError(OutOfPhlogistonsError)
    }

    implicit val errorLog = new ErrorLog()
    implicit val rand     = Blake2b512Random(128)
    implicit val costAlg  = CostAccountingAlg.unsafe[Task](CostAccount(1000))
    val reducer           = new DebruijnInterpreter[Task, Task.Par](tuplespaceAlg, Map.empty)
    val send              = Send(Channel(Quote(GString("x"))), Seq(Par()))
    val test              = reducer.inj(send).attempt.runSyncUnsafe(1.second)
    assert(test === Left(OutOfPhlogistonsError))
  }

  it should "update the cost account after going back from RSpace" in {
    pending
  }

  it should "stop interpreter threads as soon as deploy runs out of phlo" in {
    pending
  }
}
