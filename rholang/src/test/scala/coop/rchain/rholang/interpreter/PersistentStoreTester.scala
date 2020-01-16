package coop.rchain.rholang.interpreter

import java.nio.file.Files

import cats.effect.concurrent.Ref
import cats.syntax.all.none
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
import coop.rchain.rholang.interpreter.storage._
import org.scalatest.Matchers

final case class TestFixture(space: RhoISpace[Task], reducer: DebruijnInterpreter[Task, Task.Par])

trait PersistentStoreTester extends Matchers {
  implicit val ms: Metrics.Source = Metrics.BaseSource

  def withTestSpace[R](expectedError: Option[Throwable] = None)(f: TestFixture => R): R = {
    val dbDir                              = Files.createTempDirectory("rholang-interpreter-test-")
    implicit val logF: Log[Task]           = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
    implicit val noopSpan: Span[Task]      = NoopSpan[Task]()
    implicit val errLog                    = Ref.unsafe[Task, Option[Throwable]](none)
    implicit val cost                      = CostAccounting.emptyCost[Task].unsafeRunSync
    implicit val m                         = matchListPar[Task]
    val space = RSpace
      .create[
        Task,
        Par,
        BindPattern,
        ListParWithRandom,
        TaggedContinuation
      ](dbDir, 1024L * 1024L * 1024L, Branch("test"))
      .unsafeRunSync
    val reducer = RholangOnlyDispatcher.create[Task, Task.Par](space)._2
    cost.set(Cost.UNSAFE_MAX).runSyncUnsafe(1.second)
    try {
      val result = f(TestFixture(space, reducer))
      errLog.get.runSyncUnsafe(1.second) shouldBe expectedError
      result
    } finally {
      space.close()
      dbDir.recursivelyDelete()
    }
  }

  def fixture[R](f: (RhoISpace[Task], Reduce[Task]) => Task[R]): R = {
    implicit val logF: Log[Task]           = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
    implicit val noopSpan: Span[Task]      = NoopSpan[Task]()
    mkRhoISpace[Task]("rholang-interpreter-test-")
      .use { rspace =>
        for {
          cost          <- CostAccounting.emptyCost[Task]
          errorReporter <- Ref.of[Task, Option[Throwable]](none)
          reducer = {
            implicit val c = cost
            implicit val e = errorReporter
            RholangOnlyDispatcher.create[Task, Task.Par](rspace)._2
          }
          _   <- cost.set(Cost.UNSAFE_MAX)
          res <- f(rspace, reducer)
        } yield res
      }
      .runSyncUnsafe(3.seconds)
  }
}
