package coop.rchain.rholang.interpreter

import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rholang.Resources.mkRhoISpace
import coop.rchain.rholang.interpreter.Runtime.RhoISpace
import coop.rchain.rholang.interpreter.accounting.{Cost, CostAccounting}
import coop.rchain.rholang.interpreter.error_handling.{_error, ErrorHandling}
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.Matchers

import scala.concurrent.duration._

final case class TestFixture(space: RhoISpace[Task], reducer: ChargingReducer[Task])

trait PersistentStoreTester extends Matchers {

  def fixture[R](
      expectedError: Option[Throwable] = None
  )(f: (RhoISpace[Task], ChargingReducer[Task]) => Task[R]): R = {
    implicit val logF: Log[Task]           = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
    implicit val noopSpan: Span[Task]      = NoopSpan[Task]()
    mkRhoISpace[Task]("rholang-interpreter-test-")
      .use { rspace =>
        for {
          cost  <- CostAccounting.emptyCost[Task]
          error <- ErrorHandling.emptyError[Task]
          reducer = {
            implicit val c = cost
            implicit val e = error
            RholangOnlyDispatcher.create[Task, Task.Par](rspace)._2
          }
          _           <- reducer.setPhlo(Cost.UNSAFE_MAX)
          res         <- f(rspace, reducer)
          actualError <- error.get
          _           = assert(actualError === expectedError)
        } yield res
      }
      .runSyncUnsafe(3.seconds)
  }
}
