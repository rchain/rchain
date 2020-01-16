package coop.rchain.rholang.interpreter

import cats.effect.concurrent.Ref
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.Expr.ExprInstance.{EVarBody, GString}
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import cats.syntax.all._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.Resources.mkRhoISpace
import coop.rchain.rholang.interpreter.Runtime.RhoISpace
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.storage.ISpaceStub
import coop.rchain.rholang.interpreter.storage._
import coop.rchain.rspace._
import coop.rchain.rspace.internal.{Datum, Row}
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalactic.TripleEqualsSupport
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class CostAccountingReducerTest extends FlatSpec with Matchers with TripleEqualsSupport {

  implicit val noopSpan: Span[Task]   = Span.noop
  implicit val metrics: Metrics[Task] = new Metrics.MetricsNOP[Task]
  implicit val ms: Metrics.Source     = Metrics.BaseSource

  behavior of "Cost accounting in Reducer"

  it should "charge for the successful substitution" in {
    val term: Expr => Par = expr => Par(bundles = Seq(Bundle(Par(exprs = Seq(expr)))))
    val substTerm         = term(Expr(GString("1")))
    val termCost          = Chargeable[Par].cost(substTerm)
    val initCost          = Cost(1000)
    (for {
      cost          <- CostAccounting.initialCost[Task](initCost)
      errorReporter <- Ref.of[Task, Option[Throwable]](none)
      _ <- {
        implicit val c = cost
        implicit val e = errorReporter
        Substitute.charge(Task.now(substTerm), Cost(10000))
      }
      errorOption <- errorReporter.get
      _           = assert(errorOption === none)
      finalCost   <- cost.get
      _           = assert(finalCost === (initCost - Cost(termCost)))
    } yield ()).runSyncUnsafe(5.seconds)
  }

  it should "charge for failed substitution" in {
    val term: Expr => Par = expr => Par(bundles = Seq(Bundle(Par(exprs = Seq(expr)))))
    val varTerm           = term(Expr(EVarBody(EVar(Var(FreeVar(0))))))
    val originalTermCost  = Chargeable[Par].cost(varTerm)
    val initCost          = Cost(1000)

    (for {
      cost          <- CostAccounting.initialCost[Task](initCost)
      errorReporter <- Ref.of[Task, Option[Throwable]](none)
      res <- {
        implicit val c = cost
        implicit val e = errorReporter
        Substitute
          .charge(Task.raiseError[Par](new RuntimeException("")), Cost(originalTermCost))
          .attempt
      }
      _         = assert(res.isLeft)
      finalCost <- cost.get
      _         = assert(finalCost === (initCost - Cost(originalTermCost)))
    } yield ()).runSyncUnsafe(5.seconds)
  }

  it should "stop interpreter threads as soon as deploy runs out of phlo" ignore {
    // Given
    // new x in { @x!("a") | @x!("b") }
    // and not enough phlos to reduce successfully
    // only one of the branches we should be persisted in the tuplespace

    val channel: Par = GPrivateBuilder("x")

    val a: Par = GString("a")
    val b: Par = GString("b")
    val program =
      Par(sends = Seq(Send(channel, Seq(a)), Send(channel, Seq(b))))

    implicit val rand            = Blake2b512Random(Array.empty[Byte])
    implicit val errLog          = Ref.unsafe[Task, Option[Throwable]](none)
    implicit val logF: Log[Task] = Log.log[Task]

    def testImplementation(pureRSpace: RhoISpace[Task]): Task[
      (
          Either[Throwable, Unit],
          Map[Seq[Par], Row[BindPattern, ListParWithRandom, TaggedContinuation]]
      )
    ] = {

      implicit val cost = CostAccounting.emptyCost[Task].runSyncUnsafe(1.second)

      lazy val (_, reducer) =
        RholangAndScalaDispatcher
          .create[Task, Task.Par](
            pureRSpace,
            Map.empty,
            Map.empty
          )

      def plainSendCost(p: Par): Cost = {
        val storageCost = accounting.storageCostProduce(
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
        _           <- cost.set(initPhlos)
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
      res           <- mkRhoISpace[Task]("cost-accounting-reducer-test-").use(testImplementation(_))
      (result, map) = res
      errors        <- errLog.get
      _             = assert(errors === Some(OutOfPhlogistonsError))
      _             = assert(stored(map, a, rand.splitByte(0)) || stored(map, b, rand.splitByte(1)))
    } yield ()).runSyncUnsafe(5.seconds)

  }
}
