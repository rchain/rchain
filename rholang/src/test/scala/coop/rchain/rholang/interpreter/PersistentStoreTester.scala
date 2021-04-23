package coop.rchain.rholang.interpreter

import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.rholang.Resources.mkRhoISpace
import coop.rchain.rholang.interpreter.RhoRuntime.RhoISpace
import coop.rchain.rholang.interpreter.accounting.{Cost, CostAccounting}
import coop.rchain.rholang.interpreter.storage._
import coop.rchain.rspace.RSpace
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import coop.rchain.shared.Log
import coop.rchain.store.InMemoryStoreManager
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.duration._

final case class TestFixture(space: RhoISpace[Task], reducer: DebruijnInterpreter[Task])

trait PersistentStoreTester {
  implicit val ms: Metrics.Source = Metrics.BaseSource

  def withTestSpace[R](f: TestFixture => R): R = {
    implicit val logF: Log[Task]           = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
    implicit val noopSpan: Span[Task]      = NoopSpan[Task]()

    implicit val cost = CostAccounting.emptyCost[Task].unsafeRunSync
    implicit val m    = matchListPar[Task]
    implicit val kvm  = InMemoryStoreManager[Task]
    val store         = kvm.rSpaceStores.unsafeRunSync
    val space = RSpace
      .create[Task, Par, BindPattern, ListParWithRandom, TaggedContinuation](store)
      .unsafeRunSync
    val reducer = RholangOnlyDispatcher.create[Task, Task.Par](space)._2
    cost.set(Cost.UNSAFE_MAX).runSyncUnsafe(1.second)

    // Execute test
    f(TestFixture(space, reducer))
  }

  def fixture[R](f: (RhoISpace[Task], Reduce[Task]) => Task[R]): R = {
    implicit val logF: Log[Task]           = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
    implicit val noopSpan: Span[Task]      = NoopSpan[Task]()
    implicit val kvm                       = InMemoryStoreManager[Task]
    mkRhoISpace[Task]
      .flatMap {
        case rspace =>
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
