package coop.rchain.rholang.interpreter

import java.nio.file.Files

import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.rholang.interpreter.Runtime.RhoISpace
import coop.rchain.rholang.interpreter.accounting.{Cost, CostAccounting}
import coop.rchain.rspace.RSpace
import coop.rchain.rspace.history.Branch
import coop.rchain.shared.Log
import monix.eval.Task
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.rholang.Resources.mkRhoISpace
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.duration._
import coop.rchain.shared.PathOps._
import coop.rchain.rholang.interpreter.storage.implicits._

final case class TestFixture(space: RhoISpace[Task], reducer: ChargingReducer[Task])

trait PersistentStoreTester {

  def withTestSpace[R](
      errorLog: ErrorLog[Task]
  )(f: TestFixture => R): R = {
    val dbDir                              = Files.createTempDirectory("rholang-interpreter-test-")
    implicit val logF: Log[Task]           = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
    implicit val noopSpan: Span[Task]      = NoopSpan[Task]()

    implicit val cost = CostAccounting.emptyCost[Task].unsafeRunSync

    val space = (RSpace
      .create[
        Task,
        Par,
        BindPattern,
        ListParWithRandom,
        TaggedContinuation
      ](dbDir, 1024L * 1024L * 1024L, Branch("test")))
      .unsafeRunSync
    implicit val errLog = errorLog
    val reducer         = RholangOnlyDispatcher.create[Task, Task.Par](space)._2
    reducer.setPhlo(Cost.UNSAFE_MAX).runSyncUnsafe(1.second)
    try {
      f(TestFixture(space, reducer))
    } finally {
      space.close()
      dbDir.recursivelyDelete()
    }
  }

  def fixture[R](f: (RhoISpace[Task], ChargingReducer[Task]) => Task[R])(
      implicit errorLog: ErrorLog[Task]
  ): R = {
    implicit val logF: Log[Task]           = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
    implicit val noopSpan: Span[Task]      = NoopSpan[Task]()
    mkRhoISpace[Task]("rholang-interpreter-test-")
      .use { rspace =>
        for {
          cost <- CostAccounting.emptyCost[Task]
          reducer = {
            implicit val c = cost
            RholangOnlyDispatcher.create[Task, Task.Par](rspace)._2
          }
          _   <- reducer.setPhlo(Cost.UNSAFE_MAX)
          res <- f(rspace, reducer)
        } yield res
      }
      .runSyncUnsafe(3.seconds)
  }
}
