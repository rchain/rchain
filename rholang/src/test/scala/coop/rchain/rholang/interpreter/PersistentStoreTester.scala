package coop.rchain.rholang.interpreter

import java.nio.file.Files

import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.rholang.interpreter.Runtime.RhoISpace
import coop.rchain.rholang.interpreter.accounting.{Cost, CostAccounting}
import coop.rchain.rspace.RSpace
import coop.rchain.shared.Log
import monix.eval.Task
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.rholang.Resources.mkRhoISpace
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.duration._
import coop.rchain.shared.PathOps._
import coop.rchain.rholang.interpreter.storage._
import coop.rchain.store.InMemoryStoreManager

final case class TestFixture(space: RhoISpace[Task], reducer: DebruijnInterpreter[Task])

trait PersistentStoreTester {
  implicit val ms: Metrics.Source = Metrics.BaseSource

  def withTestSpace[R](f: TestFixture => R): R = {
    val dbDir                              = Files.createTempDirectory("rholang-interpreter-test-")
    implicit val logF: Log[Task]           = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
    implicit val noopSpan: Span[Task]      = NoopSpan[Task]()

    implicit val cost = CostAccounting.emptyCost[Task].unsafeRunSync
    implicit val m    = matchListPar[Task]
    implicit val kvm  = InMemoryStoreManager[Task]
    val roots         = kvm.store("roots").unsafeRunSync
    val cold          = kvm.store("cold").unsafeRunSync
    val history       = kvm.store("history").unsafeRunSync
    val space = RSpace
      .create[
        Task,
        Par,
        BindPattern,
        ListParWithRandom,
        TaggedContinuation
      ](roots, cold, history)
      .unsafeRunSync
    val reducer = RholangOnlyDispatcher.create[Task, Task.Par](space)._2
    cost.set(Cost.UNSAFE_MAX).runSyncUnsafe(1.second)
    try {
      f(TestFixture(space, reducer))
    } finally {
      dbDir.recursivelyDelete()
    }
  }

  def fixture[R](f: (RhoISpace[Task], Reduce[Task]) => Task[R]): R = {
    implicit val logF: Log[Task]           = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
    implicit val noopSpan: Span[Task]      = NoopSpan[Task]()
    mkRhoISpace[Task]("rholang-interpreter-test-")
      .use {
        case (rspace, _) =>
          for {
            cost <- CostAccounting.emptyCost[Task]
            reducer = {
              implicit val c = cost
              RholangOnlyDispatcher.create[Task, Task.Par](rspace)._2
            }
            _   <- cost.set(Cost.UNSAFE_MAX)
            res <- f(rspace, reducer)
          } yield res
      }
      .runSyncUnsafe(3.seconds)
  }
}
