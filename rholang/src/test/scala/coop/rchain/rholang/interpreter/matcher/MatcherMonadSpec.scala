package coop.rchain.rholang.interpreter.matcher

import cats.effect._
import cats.mtl.implicits._
import cats.syntax.all._
import cats.{Alternative, Foldable, MonoidK, SemigroupK}
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.metrics.Metrics
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter._
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.matcher.{run => runMatcher}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

class MatcherMonadSpec extends AnyFlatSpec with Matchers {
  implicit val metrics: Metrics[Task] = new Metrics.MetricsNOP[Task]
  implicit val ms: Metrics.Source     = Metrics.BaseSource

  type F[A] = MatcherMonadT[Task, A]

  val A: Alternative[F] = Alternative[F]

  implicit val cost = CostAccounting.emptyCost[Task].unsafeRunSync

  implicit val costF: _cost[F]   = matcherMonadCostLog[Task]
  implicit val matcherMonadError = implicitly[Sync[F]]

  private def combineK[FF[_]: MonoidK, G[_]: Foldable, A](gfa: G[FF[A]]): FF[A] =
    gfa.foldLeft(MonoidK[FF].empty[A])(SemigroupK[FF].combineK[A])

  private def runWithCost[A](f: Task[A], phlo: Int) =
    (for {
      _        <- cost.set(Cost(phlo, "initial cost"))
      result   <- f
      phloLeft <- cost.get
    } yield (phloLeft, result)).unsafeRunSync

  behavior of "MatcherMonad"

  it should "charge for each non-deterministic branch" in {
    val possibleResults = Stream((0, 1), (0, 2))
    val computation     = Alternative[F].unite(possibleResults.pure[F])
    val sum             = computation.map { case (x, y) => x + y } >>= (charge[F](Cost(1)).as(_))
    val (phloLeft, _)   = runWithCost(runMatcher(sum), possibleResults.size)
    assert(phloLeft.value == 0)

    val moreVariants    = sum.flatMap(x => Alternative[F].unite(Stream(x, 0, -x).pure[F]))
    val moreComputation = moreVariants.map(x => "Do sth with " + x) >>= (charge[F](Cost(1)).as(_))
    val (phloLeft2, _) =
      runWithCost(runMatcher(moreComputation), possibleResults.size * 3 + possibleResults.size)
    assert(phloLeft2.value == 0)

  }

  val modifyStates = for {
    _ <- _freeMap[F].set(Map(42 -> Par()))
    _ <- costF.modify(_ + Cost(1))
  } yield ()

  it should "retain cost and matches when attemptOpt is called on successful match" in {
    val (phloLeft, res) = runWithCost(runFirst(attemptOpt[F, Unit](modifyStates)), 0)
    assert(phloLeft.value == 1)
    assert(res == Some((Map(42 -> Par()), Some(()))))
  }

  it should "retain cost but discard matches when attemptOpt is called on a match failed using _short" in {
    val failed = for {
      _ <- modifyStates
      _ <- _short[F].raiseError[Int](())
    } yield ()

    val (phloLeft, res) = runWithCost(runFirst(attemptOpt[F, Unit](failed)), 0)
    assert(phloLeft.value == 1)
    assert(res == Some((Map.empty, None)))

  }

  it should "retain cost but discard matches when attemptOpt is called on a match failed using `guard`" in {
    val failed = for {
      _ <- modifyStates
      _ <- A.guard(false)
    } yield ()

    val (phloLeft, res) = runWithCost(runFirst(attemptOpt[F, Unit](failed)), 0)
    assert(phloLeft.value == 1)
    assert(res == Some((Map.empty, None)))
  }

  it should "apply `guard`-s separately to each computation branch" in {
    val a: F[Int] = A.guard(true) >> 1.pure[F]
    val b: F[Int] = A.guard(false) >> 2.pure[F]
    val c: F[Int] = A.guard(true) >> 3.pure[F]
    val combined  = combineK(List(a, b, c))

    val (phloLeft, res) = runWithCost(runMatcher(combined), 0)
    assert(phloLeft.value == 0)
    assert(res == Stream((Map.empty, 1), (Map.empty, 3)))
  }

  it should "apply `_short[F].raiseError` separately to each computation branch" in {
    val a: F[Int] = _short[F].raiseError[Int](()) >> 1.pure[F]
    val b: F[Int] = 2.pure[F]
    val c: F[Int] = 3.pure[F] >> _short[F].raiseError[Int](())
    val combined  = combineK(List(a, b, c))

    val (phloLeft, res) = runWithCost(runMatcher(combined), 0)
    assert(phloLeft.value == 0)
    assert(res == Stream((Map.empty, 2)))
  }

  it should "fail all branches when using `_error[F].raise`" in {
    val a: F[Int] = 1.pure[F]
    val b: F[Int] = 2.pure[F] >> _error[F].raiseError[Int](OutOfPhlogistonsError)
    val c: F[Int] = 3.pure[F]

    val combined = combineK(List(a, b, c))
    val (_, res) = runWithCost(runMatcher(combined).attempt, 0)
    res shouldBe (Left(OutOfPhlogistonsError))
  }

  it should "charge for each branch as long as `charge` is before a short-circuit" in {
    val a: F[Unit] = charge[F](Cost(1))

    val b: F[Unit] = charge[F](Cost(2)) >> A.guard(false)
    val c: F[Unit] = A.guard(false) >> charge[F](Cost(4))

    val d: F[Unit] = charge[F](Cost(8)) >> _short[F].raiseError[Unit](())
    val e: F[Unit] = _short[F].raiseError[Int](()) >> charge[F](Cost(16))

    val combined        = combineK(List(a, b, c, d, e))
    val (phloLeft, res) = runWithCost(runMatcher(combined), 1 + 2 + 8)

    assert(phloLeft.value == 0)
    assert(res == Stream((Map.empty, ())))

  }

}
