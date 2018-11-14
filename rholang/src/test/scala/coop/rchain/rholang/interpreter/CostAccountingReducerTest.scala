package coop.rchain.rholang.interpreter

import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Expr.ExprInstance.{EVarBody, GString}
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Reduce.DebruijnInterpreter
import coop.rchain.rholang.interpreter.accounting.{Chargeable, Cost, CostAccount, CostAccounting, _}
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rholang.interpreter.storage.{ChargingRSpace, ChargingRSpaceTest, Tuplespace}
import coop.rchain.rspace.internal.{Datum, Row}
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler.Implicits.global
import org.scalactic.TripleEqualsSupport
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.rholang.Resources.mkRhoISpace
import coop.rchain.rholang.interpreter.Runtime.RhoISpace
import coop.rchain.rspace.internal

import scala.collection.immutable
import scala.concurrent.duration._

class CostAccountingReducerTest extends FlatSpec with Matchers with TripleEqualsSupport {

  behavior of "Cost accounting in Reducer"

  it should "charge for the successful substitution" in {
    val term: Expr => Par = expr => Par(bundles = Seq(Bundle(Par(exprs = Seq(expr)))))
    val substTerm         = term(Expr(GString("1")))
    val termCost          = Chargeable[Par].cost(substTerm)
    val initCost          = CostAccount(1000)
    implicit val costAlg  = CostAccounting.unsafe[Coeval](initCost)
    val res               = Substitute.charge(Coeval.pure(substTerm), Cost(10000)).attempt.value
    assert(res === Right(substTerm))
    assert(costAlg.get().value.cost === (initCost.cost - Cost(termCost)))
  }

  it should "charge for failed substitution" in {
    val term: Expr => Par = expr => Par(bundles = Seq(Bundle(Par(exprs = Seq(expr)))))
    val varTerm           = term(Expr(EVarBody(EVar(Var(FreeVar(0))))))
    val originalTermCost  = Chargeable[Par].cost(varTerm)
    val initCost          = CostAccount(1000)
    implicit val costAlg  = CostAccounting.unsafe[Coeval](initCost)
    val res = Substitute
      .charge(Coeval.raiseError[Par](new RuntimeException("")), Cost(originalTermCost))
      .attempt
      .value
    assert(res.isLeft)
    assert(costAlg.get().value.cost === (initCost.cost - Cost(originalTermCost)))
  }

  it should "stop if OutOfPhloError is returned from RSpace" in {
    val tuplespaceAlg = new Tuplespace[Task] {
      override def produce(
          chan: Par,
          data: ListParWithRandom,
          persistent: Boolean,
          sequenceNumber: Int
      ): Task[Unit] =
        Task.raiseError(OutOfPhlogistonsError)
      override def consume(
          binds: Seq[(BindPattern, Par)],
          body: ParWithRandom,
          persistent: Boolean,
          sequenceNumber: Int
      ): Task[Unit] = Task.raiseError(OutOfPhlogistonsError)
    }

    implicit val errorLog = new ErrorLog()
    implicit val rand     = Blake2b512Random(128)
    implicit val costAlg  = CostAccounting.unsafe[Task](CostAccount(1000))
    val reducer           = new DebruijnInterpreter[Task, Task.Par](tuplespaceAlg, Map.empty)
    val send              = Send(Par(exprs = Seq(GString("x"))), Seq(Par()))
    val test              = reducer.inj(send).attempt.runSyncUnsafe(1.second)
    assert(test === Left(OutOfPhlogistonsError))
  }

  it should "stop interpreter threads as soon as deploy runs out of phlo" in {
    // Given
    // new x in { @x!("a") | @x!("b") }
    // and not enough phlos to reduce successfully
    // only one of the branches we should be persisted in the tuplespace

    val channel: Par = GPrivateBuilder("x")

    val a: Par = GString("a")
    val b: Par = GString("b")
    val program =
      Par(sends = Seq(Send(channel, Seq(a)), Send(channel, Seq(b))))

    implicit val rand   = Blake2b512Random(Array.empty[Byte])
    implicit val errLog = new ErrorLog()

    def testImplementation(pureRSpace: RhoISpace[Task]): Task[
      (
          Either[Throwable, Unit],
          Map[immutable.Seq[Par], Row[BindPattern, ListParWithRandom, TaggedContinuation]]
      )
    ] = {

      lazy val (_, reducer, _) =
        RholangAndScalaDispatcher.create[Task, Task.Par](pureRSpace, Map.empty, Map.empty)

      def plainSendCost(p: Par): Cost = {
        val storageCost = ChargingRSpace.storageCostProduce(
          channel,
          ListParWithRandom(Seq(p))
        )
        val substitutionCost = Cost(Chargeable[Par].cost(channel)) + Cost(
          Chargeable[Par].cost(p)
        )
        substitutionCost + storageCost + SEND_EVAL_COST
      }
      val sendACost = plainSendCost(a)
      val sendBCost = plainSendCost(b)

      val initPhlos = sendACost + sendBCost - SEND_EVAL_COST - Cost(1)

      for {
        _      <- reducer.setAvailablePhlos(initPhlos)
        result <- reducer.inj(program).attempt
      } yield (result, pureRSpace.store.toMap)
    }

    val (result, map) =
      mkRhoISpace[Task]("cost-accounting-reducer-test-")
        .use(testImplementation(_))
        .runSyncUnsafe(5.seconds)

    val errors = errLog.readAndClearErrorVector()

    def data(p: Par, rand: Blake2b512Random) = Row(
      List(
        Datum.create(
          channel,
          ListParWithRandom(Seq(p), rand),
          false
        )
      ),
      List()
    )

    def stored(
        map: Map[immutable.Seq[Par], Row[BindPattern, ListParWithRandom, TaggedContinuation]],
        p: Par,
        rand: Blake2b512Random
    ): Boolean =
      map.get(List(channel)) === Some(data(p, rand))

    assert(errors === Vector.empty)
    assert(result === Left(OutOfPhlogistonsError))
    assert(stored(map, a, rand.splitByte(0)) || stored(map, b, rand.splitByte(1)))
  }
}
