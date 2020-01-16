package coop.rchain.rholang.interpreter.matcher

import cats._
import cats.data._
import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import cats.mtl.implicits._
import cats.{Alternative, Foldable, Functor, MonoidK, SemigroupK}

import coop.rchain.catscontrib.mtl.implicits._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.metrics.Metrics
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter._
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.accounting.CostAccounting._
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.matcher.{run => runMatcher, _}

import org.scalatest._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

class MatcherMonadSpec extends FlatSpec with Matchers {
  implicit val metrics: Metrics[Task] = new Metrics.MetricsNOP[Task]
  implicit val ms: Metrics.Source     = Metrics.BaseSource

  type F[A] = MatcherMonadT[Task, A]

  val A: Alternative[F] = Alternative[F]

  implicit val cost = CostAccounting.emptyCost[Task].unsafeRunSync

  implicit val costF: _cost[F]   = matcherMonadCostLog[Task]
  implicit val matcherMonadError = implicitly[Sync[F]]

  private def combineK[FF[_]: MonoidK, G[_]: Foldable, A](gfa: G[FF[A]]): FF[A] =
    gfa.foldLeft(MonoidK[FF].empty[A])(SemigroupK[FF].combineK[A])

  behavior of "MatcherMonad"

  private val modifyStates = _freeMap[F].set(Map(42 -> Par()))

  it should "retain matches when attemptOpt is called on successful match" in {
    val res = runFirst(attemptOpt[F, Unit](modifyStates)).unsafeRunSync
    assert(res == Some((Map(42 -> Par()), Some(()))))
  }

  it should "discard matches when attemptOpt is called on a match failed using _short" in {
    val failed = for {
      _ <- modifyStates
      _ <- _short[F].raiseError[Int](())
    } yield ()

    val res = runFirst(attemptOpt[F, Unit](failed)).unsafeRunSync
    assert(res == Some((Map.empty, None)))
  }

  it should "discard matches when attemptOpt is called on a match failed using `guard`" in {
    val failed = for {
      _ <- modifyStates
      _ <- A.guard(false)
    } yield ()

    val res = runFirst(attemptOpt[F, Unit](failed)).unsafeRunSync
    assert(res == Some((Map.empty, None)))
  }

  it should "apply `guard`-s separately to each computation branch" in {
    val a: F[Int] = A.guard(true) >> 1.pure[F]
    val b: F[Int] = A.guard(false) >> 2.pure[F]
    val c: F[Int] = A.guard(true) >> 3.pure[F]
    val combined  = combineK(List(a, b, c))

    val res = runMatcher(combined).unsafeRunSync
    assert(res == Stream((Map.empty, 1), (Map.empty, 3)))
  }

  it should "apply `_short[F].raiseError` separately to each computation branch" in {
    val a: F[Int] = _short[F].raiseError[Int](()) >> 1.pure[F]
    val b: F[Int] = 2.pure[F]
    val c: F[Int] = 3.pure[F] >> _short[F].raiseError[Int](())
    val combined  = combineK(List(a, b, c))

    val res = runMatcher(combined).unsafeRunSync
    assert(res == Stream((Map.empty, 2)))
  }

  it should "fail all branches when using `_error[F].raise`" in {
    case object TestError extends Throwable
    val a: F[Int] = 1.pure[F]
    val b: F[Int] = 2.pure[F] >> MonadError[F, Throwable].raiseError[Int](TestError)
    val c: F[Int] = 3.pure[F]

    val combined = combineK(List(a, b, c))
    val res      = runMatcher(combined).attempt.unsafeRunSync
    res shouldBe (Left(TestError))
  }
}
