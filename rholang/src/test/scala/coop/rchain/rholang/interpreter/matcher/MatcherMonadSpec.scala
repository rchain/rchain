package coop.rchain.rholang.interpreter.matcher

import cats._
import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import cats.mtl.implicits._
import cats.{Alternative, Foldable, MonoidK, SemigroupK}
import coop.rchain.catscontrib.mtl.implicits._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter._
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.accounting.CostAccounting._
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.matcher.{run => runMatcher, _}
import coop.rchain.rholang.interpreter.matcher.NonDetFreeMapWithCost._
import org.scalatest.FlatSpec

import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

class MatcherMonadSpec extends FlatSpec {

  type F[A] = MatcherMonadT[Task, A]

  val A: Alternative[F] = Alternative[F]

  implicit val cost: _cost[Task] = CostAccounting.unsafe[Task](Cost(0))

  private def combineK[FF[_]: MonoidK, G[_]: Foldable, A](gfa: G[FF[A]]): FF[A] =
    gfa.foldLeft(MonoidK[FF].empty[A])(SemigroupK[FF].combineK[A])

  behavior of "MatcherMonad"

  it should "charge for each non-deterministic branch" in {

    val possibleResults = Stream((0, 1), (0, 2))
    val computation     = Alternative[F].unite(possibleResults.pure[F])
    val sum             = computation.map { case (x, y) => x + y } <* charge[F](Cost(1))
    val t1 = (for {
      _        <- cost.set(Cost(possibleResults.size))
      _        <- runMatcher(sum)
      phloLeft <- cost.get
    } yield (assert(phloLeft.value == 0)))

    val moreVariants    = sum.flatMap(x => Alternative[F].unite(Stream(x, 0, -x).pure[F]))
    val moreComputation = moreVariants.map(x => "Do sth with " + x) <* charge[F](Cost(1))

    val t2 = (for {
      _        <- cost.set(Cost(possibleResults.size * 3 + possibleResults.size))
      _        <- runMatcher(moreComputation)
      phloLeft <- cost.get
    } yield (assert(phloLeft.value == 0)))

    t1 >> t2 unsafeRunSync
  }

  val modifyStates = for {
    _ <- _freeMap[F].set(Map(42 -> Par()))
    _ <- _cost[F].modify(_ + Cost(1))
  } yield ()

  it should "retain cost and matches when attemptOpt is called on successful match" in {
    (for {
      _        <- cost.set(Cost(0))
      a        = attemptOpt(modifyStates)
      res      <- runFirstWithCost(a)
      (_, r)   = res
      phloLeft <- cost.get
      _        = assert(phloLeft.value == 1)
    } yield (assert(r == Some((Map(42 -> Par()), Some(())))))).unsafeRunSync
  }

  it should "retain cost but discard matches when attemptOpt is called on a match failed using _short" in {
    val failed = for {
      _ <- modifyStates
      _ <- _short[F].raiseError[Int](())
    } yield ()

    (for {
      _        <- cost.set(Cost(0))
      a        = attemptOpt[F, Unit](failed)
      res      <- runFirstWithCost(a)
      (_, r)   = res
      phloLeft <- cost.get
      _        = assert(phloLeft.value == 1)
    } yield (assert(r == Some((Map.empty, None))))).unsafeRunSync

  }

  it should "retain cost but discard matches when attemptOpt is called on a match failed using `guard`" in {
    val failed = for {
      _ <- modifyStates
      _ <- A.guard(false)
    } yield ()

    (for {
      _        <- cost.set(Cost(0))
      a        = attemptOpt[F, Unit](failed)
      res      <- runFirstWithCost(a)
      (_, r)   = res
      phloLeft <- cost.get
      _        = assert(phloLeft.value == 1)
    } yield (assert(r == Some((Map.empty, None))))).unsafeRunSync

  }

  it should "apply `guard`-s separately to each computation branch" in {
    val a: F[Int] = A.guard(true) >> 1.pure[F]
    val b: F[Int] = A.guard(false) >> 2.pure[F]
    val c: F[Int] = A.guard(true) >> 3.pure[F]

    (for {
      _        <- cost.set(Cost(0))
      combined = combineK(List(a, b, c))
      res      <- runMatcher(combined)
      phloLeft <- cost.get
      _        = assert(phloLeft.value == 0)
    } yield (assert(res == Stream((Map.empty, 1), (Map.empty, 3))))).unsafeRunSync
  }

  it should "apply `_short[F].raiseError` separately to each computation branch" in {
    val a: F[Int] = _short[F].raiseError[Int](()) >> 1.pure[F]
    val b: F[Int] = 2.pure[F]
    val c: F[Int] = 3.pure[F] >> _short[F].raiseError[Int](())

    (for {
      _        <- cost.set(Cost(0))
      combined = combineK(List(a, b, c))
      res      <- runMatcher(combined)
      phloLeft <- cost.get
      _        = assert(phloLeft.value == 0)
    } yield (assert(res == Stream((Map.empty, 2))))).unsafeRunSync
  }

  it should "fail all branches when using `_error[F].raise`" in {
    val a: F[Int] = 1.pure[F]
    val b: F[Int] = 2.pure[F] >> _error[F].raise[Int](OutOfPhlogistonsError)
    val c: F[Int] = 3.pure[F]

    val combined = combineK(List(a, b, c))
    (for {
      res <- runMatcher(combined)
    } yield (fail("Should have ran out of phlo"))).onErrorHandleWith {
      case OutOfPhlogistonsError => Task.now(succeed)
    }.unsafeRunSync
  }

  it should "charge for each branch as long as `charge` is before a short-circuit" in {
    val a: F[Unit] = charge[F](Cost(1))

    val b: F[Unit] = charge[F](Cost(2)) >> A.guard(false)
    val c: F[Unit] = A.guard(false) >> charge[F](Cost(4))

    val d: F[Unit] = charge[F](Cost(8)) >> _short[F].raiseError[Unit](())
    val e: F[Unit] = _short[F].raiseError[Int](()) >> charge[F](Cost(16))

    (for {
      _        <- cost.set(Cost(1 + 2 + 8))
      combined = combineK(List(a, b, c, d, e))
      res      <- runMatcher(combined)
      phloLeft <- cost.get
      _        = assert(phloLeft.value == 0)
    } yield (assert(res == Stream((Map.empty, ()))))).unsafeRunSync

  }

}
