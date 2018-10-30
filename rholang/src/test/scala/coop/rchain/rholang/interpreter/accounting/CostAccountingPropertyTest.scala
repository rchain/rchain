package coop.rchain.rholang.interpreter.accounting
import cats.implicits._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.rholang.interpreter._
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.{PPar, Proc}
import coop.rchain.rholang.syntax.rholang_mercury.PrettyPrinter
import coop.rchain.rholang.{PrettyPrinted, ProcGen}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalacheck.Arbitrary
import org.scalacheck.Test.Parameters
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.rholang.RSpaceTools.runInRhoISpace
import scala.concurrent.duration._

class CostAccountingPropertyTest extends FlatSpec with PropertyChecks with Matchers {
  import CostAccountingPropertyTest._

  implicit val params: Parameters = Parameters.defaultVerbose.withMinSuccessfulTests(1000)
  implicit val procArbitrary: Arbitrary[PrettyPrinted[Proc]] = Arbitrary(
    ProcGen.topLevelGen(5).map(PrettyPrinted[Proc](_, PrettyPrinter.print))
  )

  implicit val taskExecutionDuration: FiniteDuration = 5.seconds

  def cost(proc: Proc): Cost = Cost(Interpreter.buildPar(proc).apply)

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
}

object CostAccountingPropertyTest {
  def haveEqualResults[A](tasks: Task[A]*)(implicit duration: Duration): Boolean =
    tasks.toList
      .sequence[Task, A]
      .map { _.sliding(2).forall { case List(r1, r2) => r1 == r2 } }
      .runSyncUnsafe(duration)

  def execute(reducer: ChargingReducer[Task], p: Proc)(
      implicit rand: Blake2b512Random
  ): Task[Long] = {
    val program = Interpreter.buildPar(p).apply

    val initPhlos = Cost(accounting.MAX_VALUE)

    for {
      _         <- reducer.setAvailablePhlos(initPhlos)
      _         <- reducer.inj(program)
      phlosLeft <- reducer.getAvailablePhlos()
    } yield (initPhlos - phlosLeft.cost).value
  }

  def costOfExecution(procs: Proc*): Task[Long] = {
    implicit val rand: Blake2b512Random = Blake2b512Random(Array.empty[Byte])
    implicit val errLog: ErrorLog       = new ErrorLog()

    runInRhoISpace[Task, Long](
      pureRSpace => {
        lazy val (_, reducer, _) =
          RholangAndScalaDispatcher.create[Task, Task.Par](pureRSpace, Map.empty, Map.empty)

        procs.toStream
          .traverse(execute(reducer, _))
          .map(_.sum)
      },
      prefix = "cost-accounting-property-test-"
    )

  }

}
