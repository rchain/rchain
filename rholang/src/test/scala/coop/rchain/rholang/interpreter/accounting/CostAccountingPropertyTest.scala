package coop.rchain.rholang.interpreter.accounting

import java.nio.file.Files

import cats._
import cats.effect._
import cats.implicits._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
import coop.rchain.models._
import coop.rchain.rholang.Resources.mkRhoISpace
import coop.rchain.rholang.interpreter.Runtime.{RhoContext, RhoISpace}
import coop.rchain.rholang.interpreter._
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.{PPar, Proc}
import coop.rchain.rholang.syntax.rholang_mercury.PrettyPrinter
import coop.rchain.rholang.{GenTools, ProcGen}
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.{Context, RSpace}
import coop.rchain.shared.Log
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler.Implicits.global
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Test.Parameters
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

  def cost(proc: Proc): Cost = Cost(Interpreter[Coeval].buildPar(proc).apply)

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

  def execute[F[_]: Sync](runtime: Runtime[F], p: Proc): F[Long] = {
    val interpreter = Interpreter[F]

    for {
      program <- interpreter.buildPar(p)
      res     <- interpreter.evaluate(runtime, program)
      cost    = res.cost
    } yield cost.value
  }

  def costOfExecution(procs: Proc*): Task[Long] = {
    implicit val logF: Log[Task]            = new Log.NOPLog[Task]
    implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]

    for {
      runtime <- TestRuntime.create[Task, Task.Par]()
      _       <- Runtime.injectEmptyRegistryRoot[Task](runtime.space, runtime.replaySpace)
      res <- procs.toStream
              .traverse(execute(runtime, _))
              .map(_.sum)
    } yield res

  }

}
