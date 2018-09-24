package coop.rchain.rholang.interpreter

import java.io.StringReader

import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Channel.ChannelInstance
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.Expr.ExprInstance.{EVarBody, GString}
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar}
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Reduce.DebruijnInterpreter
import coop.rchain.rholang.interpreter.accounting.{
  Chargeable,
  Cost,
  CostAccount,
  CostAccountingAlg,
  _
}
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rholang.interpreter.storage.{ChargingRSpace, ChargingRSpaceTest, TuplespaceAlg}
import coop.rchain.rspace.internal.{Datum, Row}
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler.Implicits.global
import org.scalactic.TripleEqualsSupport
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

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

  it should "stop interpreter threads as soon as deploy runs out of phlo" in {
    // we want to make two contracts run in parallel where the shorter one
    // will use up all the phlos before 2nd will hit the RSpace. We want to test
    // that only first program will change the Tuplespace and 2nd one fails with OOPE.
    //
    // @"x"!("a") | new x in { @"x"!("a" ++ "a" ++ "a" ++ … ++ "a") }

    val channel      = Channel(Quote(GString("x")))
    val depth        = 500
    val stringConcat = Seq.fill(depth)("\"a\"").mkString("++")
    val plainSendStorageCost = ChargingRSpace.storageCostProduce(
      channel,
      ListChannelWithRandom(Seq(Channel(Quote(GString("a")))))
    )
    val nestedSendStorageCost = ChargingRSpace.storageCostProduce(
      channel,
      ListChannelWithRandom(Seq(Channel(Quote(GString("a" * depth)))))
    )

    val raw =
      s"""
        new x in { @"x"!($stringConcat) } | @"x"!("a")
      """

    val program = Interpreter.buildNormalizedTerm(new StringReader(raw)).value

    implicit val rand   = Blake2b512Random(Array.empty[Byte])
    implicit val errLog = new ErrorLog()
    lazy val pureRSpace = ChargingRSpaceTest.createRhoISpace()
    lazy val (_, reducer, _) =
      RholangAndScalaDispatcher.create[Task, Task.Par](pureRSpace, Map.empty, Map.empty)

    val plainSendCost = CHANNEL_EVAL_COST + Cost(Chargeable[Channel].cost(channel)) + plainSendStorageCost + SEND_EVAL_COST
    val appendCost    = STRING_APPEND_COST * (2 to depth).sum
    val nestedSendCost = newBindingsCost(1) + CHANNEL_EVAL_COST +
      Cost(Chargeable[Channel].cost(channel)) + Cost(
      Chargeable[Channel].cost(Channel(Quote(GString("a" * depth))))
    ) +
      OP_CALL_COST * (depth - 1) +
      appendCost +
      nestedSendStorageCost + SEND_EVAL_COST
    val initPhlos = plainSendCost + nestedSendCost - SEND_EVAL_COST
    reducer.setAvailablePhlos(initPhlos).runSyncUnsafe(1.second)

    val test   = reducer.inj(program)
    val result = test.attempt.runSyncUnsafe(5.seconds)
    val store  = pureRSpace.store.toMap
    val x      = store.get(List(channel))

    val left = reducer.getAvailablePhlos().runSyncUnsafe(1.second)
    pureRSpace.close()

    assert(result === Left(OutOfPhlogistonsError))
    x shouldBe Some(
      Row(
        List(
          Datum.create(
            channel,
            ListChannelWithRandom(Seq(Channel(Quote(GString("a")))), rand.splitByte(0)),
            false
          )
        ),
        List()
      )
    )

  }
}
