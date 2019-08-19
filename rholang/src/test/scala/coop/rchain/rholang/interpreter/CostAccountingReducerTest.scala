package coop.rchain.rholang.interpreter

import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.Expr.ExprInstance.{EVarBody, GString}
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.Resources.mkRhoISpace
import coop.rchain.rholang.interpreter.Runtime.RhoISpace
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.error_handling.ErrorHandling
import coop.rchain.rholang.interpreter.error_handling.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.storage.{ChargingRSpace, ISpaceStub}
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace.internal.{Datum, Row}
import coop.rchain.rspace._
import coop.rchain.rspace.Match
import coop.rchain.shared.Log
import coop.rchain.{metrics, rspace}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalactic.TripleEqualsSupport
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.SortedSet
import scala.concurrent.duration._

class CostAccountingReducerTest extends FlatSpec with Matchers with TripleEqualsSupport {

  implicit val noopSpan: Span[Task] = Span.noop

  behavior of "Cost accounting in Reducer"

  it should "charge for the successful substitution" in {
    val term: Expr => Par = expr => Par(bundles = Seq(Bundle(Par(exprs = Seq(expr)))))
    val substTerm         = term(Expr(GString("1")))
    val termCost          = Chargeable[Par].cost(substTerm)
    val initCost          = Cost(1000)
    (for {
      cost <- CostAccounting.initialCost[Task](initCost)
      res <- {
        implicit val c = cost
        Substitute.charge(Task.now(substTerm), Cost(10000)).attempt
      }
      _         = assert(res === Right(substTerm))
      finalCost <- cost.get
      _         = assert(finalCost === (initCost - Cost(termCost)))
    } yield ()).runSyncUnsafe(5.seconds)
  }

  it should "charge for failed substitution" in {
    val term: Expr => Par = expr => Par(bundles = Seq(Bundle(Par(exprs = Seq(expr)))))
    val varTerm           = term(Expr(EVarBody(EVar(Var(FreeVar(0))))))
    val originalTermCost  = Chargeable[Par].cost(varTerm)
    val initCost          = Cost(1000)

    (for {
      cost <- CostAccounting.initialCost[Task](initCost)
      res <- {
        implicit val c = cost
        Substitute
          .charge(Task.raiseError[Par](new RuntimeException("")), Cost(originalTermCost))
          .attempt
      }
      _         = assert(res.isLeft)
      finalCost <- cost.get
      _         = assert(finalCost === (initCost - Cost(originalTermCost)))
    } yield ()).runSyncUnsafe(5.seconds)
  }

  it should "stop if OutOfPhloError is returned from RSpace" in {

    val iSpace = new ISpaceStub[
      Task,
      Par,
      BindPattern,
      ListParWithRandom,
      TaggedContinuation
    ] {
      override def produce(
          channel: Par,
          data: ListParWithRandom,
          persist: Boolean,
          sequenceNumber: Int
      )(
          implicit m: Match[Task, BindPattern, ListParWithRandom]
      ): Task[
        Option[(ContResult[Par, BindPattern, TaggedContinuation], Seq[Result[ListParWithRandom]])]
      ] =
        Task.raiseError[Option[
          (ContResult[Par, BindPattern, TaggedContinuation], Seq[Result[ListParWithRandom]])
        ]](OutOfPhlogistonsError)
    }
    implicit val error          = ErrorHandling.emptyError[Task].runSyncUnsafe(1.second)
    implicit val rand           = Blake2b512Random(128)
    implicit val cost           = CostAccounting.initialCost[Task](Cost(1000)).runSyncUnsafe(1.second)
    val (_, chargingReducer, _) = RholangAndScalaDispatcher.create(iSpace, Map.empty, Map.empty)
    val send                    = Send(Par(exprs = Seq(GString("x"))), Seq(Par()))
    chargingReducer.inj(send).attempt.runSyncUnsafe(1.second)
    val runtimeError = error.get.runSyncUnsafe(1.second)
    assert(runtimeError.contains(OutOfPhlogistonsError))
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

    implicit val rand                       = Blake2b512Random(Array.empty[Byte])
    implicit val error                      = ErrorHandling.emptyError[Task].runSyncUnsafe(1.second)
    implicit val logF: Log[Task]            = Log.log[Task]
    implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]

    def testImplementation(pureRSpace: RhoISpace[Task]): Task[
      (
          Either[Throwable, Unit],
          Map[Seq[Par], Row[BindPattern, ListParWithRandom, TaggedContinuation]]
      )
    ] = {

      implicit val cost = CostAccounting.emptyCost[Task].runSyncUnsafe(1.second)

      lazy val (_, reducer, _) =
        RholangAndScalaDispatcher
          .create[Task, Task.Par](
            pureRSpace,
            Map.empty,
            Map.empty
          )

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
        _           <- reducer.setPhlo(initPhlos)
        result      <- reducer.inj(program).attempt
        mappedSpace <- pureRSpace.toMap
      } yield (result, mappedSpace)
    }

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
        map: Map[Seq[Par], Row[BindPattern, ListParWithRandom, TaggedContinuation]],
        p: Par,
        rand: Blake2b512Random
    ): Boolean =
      map.get(List(channel)) === Some(data(p, rand))

    (for {
      res      <- mkRhoISpace[Task]("cost-accounting-reducer-test-").use(testImplementation)
      (_, map) = res
      errors   <- error.get
      _        = assert(errors.contains(OutOfPhlogistonsError))
      _        = assert(stored(map, a, rand.splitByte(0)) || stored(map, b, rand.splitByte(1)))
    } yield ()).runSyncUnsafe(5.seconds)

  }
}
