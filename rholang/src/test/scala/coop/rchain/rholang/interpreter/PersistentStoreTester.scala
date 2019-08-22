package coop.rchain.rholang.interpreter

import cats.effect.concurrent.Ref
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rholang.Resources.mkRhoISpace
import coop.rchain.rholang.interpreter.Runtime.RhoISpace
import coop.rchain.rholang.interpreter.accounting.{Cost, CostAccounting}
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.duration._

trait PersistentStoreTester {

  def fixture[R](f: (RhoISpace[Task], Reduce[Task]) => Task[R]): R = {
    implicit val logF: Log[Task]           = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
    implicit val noopSpan: Span[Task]      = NoopSpan[Task]()
    mkRhoISpace[Task]("rholang-interpreter-test-")
      .use { space =>
        for {
          cost     <- CostAccounting.emptyCost[Task]
          errorRef <- Ref.of[Task, Option[Throwable]](None)
          reducer = {
            implicit val c = cost
            RholangOnlyDispatcher.create[Task, Task.Par](space, errorRef)._2
          }
          _   <- cost.set(Cost.UNSAFE_MAX)
          res <- f(space, reducer)
        } yield res
      }
      .runSyncUnsafe(3.seconds)
  }
}
