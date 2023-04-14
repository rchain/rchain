package coop.rchain.rholang.interpreter

import cats.effect.IO
import cats.effect.unsafe.implicits.global
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

import scala.concurrent.duration._

final case class TestFixture(space: RhoISpace[IO], reducer: DebruijnInterpreter[IO])

trait PersistentStoreTester {
  implicit val ms: Metrics.Source = Metrics.BaseSource

  def withTestSpace[R](f: TestFixture => R): R = {
    implicit val logF: Log[IO]           = new Log.NOPLog[IO]
    implicit val metricsEff: Metrics[IO] = new metrics.Metrics.MetricsNOP[IO]
    implicit val noopSpan: Span[IO]      = NoopSpan[IO]()

    implicit val cost = CostAccounting.emptyCost[IO].unsafeRunSync
    implicit val m    = matchListPar[IO]
    implicit val kvm  = InMemoryStoreManager[IO]
    val store         = kvm.rSpaceStores.unsafeRunSync
    val space = RSpace
      .create[IO, Par, BindPattern, ListParWithRandom, TaggedContinuation](store, rholangEC)
      .unsafeRunSync
    val reducer = RholangOnlyDispatcher(space)._2
    cost.set(Cost.UNSAFE_MAX).unsafeRunSync

    // Execute test
    f(TestFixture(space, reducer))
  }

  def fixture[R](f: (RhoISpace[IO], Reduce[IO]) => IO[R]): R = {
    implicit val logF: Log[IO]           = new Log.NOPLog[IO]
    implicit val metricsEff: Metrics[IO] = new metrics.Metrics.MetricsNOP[IO]
    implicit val noopSpan: Span[IO]      = NoopSpan[IO]()
    implicit val kvm                     = InMemoryStoreManager[IO]
    mkRhoISpace[IO].flatMap {
      case rspace =>
        for {
          cost <- CostAccounting.emptyCost[IO]
          reducer = {
            implicit val c = cost
            RholangOnlyDispatcher(rspace)._2
          }
          _   <- cost.set(Cost.UNSAFE_MAX)
          res <- f(rspace, reducer)
        } yield res
    }.unsafeRunSync
  }
}
