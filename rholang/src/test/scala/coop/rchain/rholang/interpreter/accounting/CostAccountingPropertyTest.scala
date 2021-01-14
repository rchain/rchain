package coop.rchain.rholang.interpreter.accounting

import cats._
import cats.effect._
import cats.syntax.all._
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models._
import coop.rchain.rholang.Resources._
import coop.rchain.rholang.interpreter.{PrettyPrinter => PP, _}
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.{PPar, Proc}
import coop.rchain.rholang.syntax.rholang_mercury.PrettyPrinter
import coop.rchain.rholang.{GenTools, ProcGen}
import coop.rchain.shared.Log
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler.Implicits.global
import org.scalacheck.Test.Parameters
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class CostAccountingPropertyTest extends FlatSpec with PropertyChecks with Matchers {
  import CostAccountingPropertyTest._

  implicit val params: Parameters = Parameters.defaultVerbose.withMinSuccessfulTests(1000)

  def procGen(maxHeight: Int): Gen[PrettyPrinted[Proc]] =
    ProcGen.topLevelGen(maxHeight).map(PrettyPrinted[Proc](_, PrettyPrinter.print))
  implicit val procArbitrary: Arbitrary[PrettyPrinted[Proc]] = Arbitrary(procGen(5))

  implicit val taskExecutionDuration: FiniteDuration = 5.seconds

  def cost(proc: Proc): Cost = Cost(ParBuilderUtil.buildPar[Coeval](proc).apply)

  behavior of "Cost accounting in Reducer"

  it should "have positive cost of substitution" in {
    forAll { p: PrettyPrinted[Proc] =>
      cost(p.value).value should be >= 0L
    }
  }

  behavior of "Charging phlos in Reducer"

  it should "charge the same if the evaluation order is reversed" in {
    forAll { (p: PrettyPrinted[Proc], q: PrettyPrinted[Proc]) =>
      haveEqualResults(costOfExecution(p.value, q.value), costOfExecution(q.value, p.value))
    }
  }

  it should "sequential execution and parallel execution have the same cost" in {
    forAll { (p: PrettyPrinted[Proc], q: PrettyPrinted[Proc]) =>
      haveEqualResults(
        costOfExecution(new PPar(p.value, q.value)),
        costOfExecution(q.value, p.value)
      )
    }
  }

  it should "repeated executions have the same cost" in {
    implicit val procListArb: Arbitrary[List[PrettyPrinted[Proc]]] =
      Arbitrary(GenTools.nonemptyLimitedList(5, procGen(5)))

    forAll { ps: List[PrettyPrinted[Proc]] =>
      val costs = 1.to(10).map(_ => costOfExecution(ps.map(_.value): _*))

      haveEqualResults(costs: _*)(30.seconds)
    }
  }

}

object CostAccountingPropertyTest {

  def haveEqualResults[A](tasks: Task[A]*)(implicit duration: Duration): Boolean =
    tasks.toList
      .sequence[Task, A]
      .map { _.sliding(2).forall { case List(r1, r2) => r1 == r2 } }
      .runSyncUnsafe(duration)

  def execute[F[_]: Sync: _cost](runtime: Runtime[F], p: Proc): F[Long] =
    for {
      program <- ParBuilderUtil.buildPar(p)
      res     <- evaluatePar(runtime, program)
      cost    = res.cost
    } yield cost.value

  def evaluatePar[F[_]: Monad: Sync: _cost](
      runtime: Runtime[F],
      par: Par
  ): F[EvaluateResult] = {
    val term = PP().buildString(par)

    InterpreterUtil.evaluateResult(runtime, term)
  }

  def costOfExecution(procs: Proc*): Task[Long] = {
    implicit val logF: Log[Task]            = new Log.NOPLog[Task]
    implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
    implicit val noopSpan: Span[Task]       = NoopSpan[Task]()
    implicit val ms: Metrics.Source         = Metrics.BaseSource

    val prefix = "cost-accounting-property-test"
    mkRuntime[Task](prefix).use { runtime =>
      for {
        _    <- runtime.cost.set(Cost.UNSAFE_MAX)
        cost <- CostAccounting.emptyCost[Task]
        res <- {
          implicit val c = cost
          procs.toStream
            .traverse(execute(runtime, _))
            .map(_.sum)
        }
      } yield res

    }
  }

}
