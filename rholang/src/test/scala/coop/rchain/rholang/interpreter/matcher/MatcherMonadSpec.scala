package coop.rchain.rholang.interpreter.matcher

import cats.implicits._
import cats.mtl.implicits._
import cats.{Alternative, Foldable, MonoidK, SemigroupK}
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.matcher.NonDetFreeMapWithCost._
import org.scalatest.FlatSpec

class MatcherMonadSpec extends FlatSpec {

  type F[A] = NonDetFreeMapWithCost[A]

  val A: Alternative[F] = Alternative[F]

  private def combineK[FF[_]: MonoidK, G[_]: Foldable, A](gfa: G[FF[A]]): FF[A] =
    gfa.foldLeft(MonoidK[FF].empty[A])(SemigroupK[FF].combineK[A])

  behavior of "MatcherMonad"

  it should "charge for each non-deterministic branch" in {

    val possibleResults = Stream((0, 1), (0, 2))
    val computation     = Alternative[F].unite(possibleResults.pure[F])
    val sum             = computation.map { case (x, y) => x + y } <* charge[F](Cost(1))
    val (cost, _)       = sum.runWithCost(Cost(possibleResults.size)).right.get
    assert(cost.value == 0)

    val moreVariants    = sum.flatMap(x => Alternative[F].unite(Stream(x, 0, -x).pure[F]))
    val moreComputation = moreVariants.map(x => "Do sth with " + x) <* charge[F](Cost(1))
    val (cost2, _) =
      moreComputation.runWithCost(Cost(possibleResults.size * 3 + possibleResults.size)).right.get
    assert(cost2.value == 0)
  }

  val modifyStates = for {
    _ <- _freeMap[F].set(Map(42 -> Par()))
    _ <- _cost[F].modify(_ + Cost(1))
  } yield ()

  it should "retain cost and matches when attemptOpt is called on successful match" in {
    assert(
      attemptOpt[F, Unit](modifyStates)
        .runFirstWithCost(Cost(0)) == Right((Cost(1), Some((Map(42 -> Par()), Some(())))))
    )
  }

  it should "retain cost but discard matches when attemptOpt is called on a match failed using _short" in {
    val failed = for {
      _ <- modifyStates
      _ <- _short[F].raiseError[Int](())
    } yield ()

    assert(
      attemptOpt[F, Unit](failed)
        .runFirstWithCost(Cost(0)) == Right((Cost(1), Some((Map.empty, None))))
    )
  }

  it should "retain cost but discard matches when attemptOpt is called on a match failed using `guard`" in {
    val failed = for {
      _ <- modifyStates
      _ <- A.guard(false)
    } yield ()

    assert(
      attemptOpt[F, Unit](failed)
        .runFirstWithCost(Cost(0)) == Right((Cost(1), Some((Map.empty, None))))
    )
  }

  it should "apply `guard`-s separately to each computation branch" in {
    val a: F[Int] = A.guard(true) >> 1.pure[F]
    val b: F[Int] = A.guard(false) >> 2.pure[F]
    val c: F[Int] = A.guard(true) >> 3.pure[F]

    val result = combineK(List(a, b, c)).runWithCost(Cost(0))
    assert(result.right.get._2 == Stream((Map.empty, 1), (Map.empty, 3)))
  }

  it should "apply `_short[F].raiseError` separately to each computation branch" in {
    val a: F[Int] = _short[F].raiseError[Int](()) >> 1.pure[F]
    val b: F[Int] = 2.pure[F]
    val c: F[Int] = 3.pure[F] >> _short[F].raiseError[Int](())

    val result = combineK(List(a, b, c)).runWithCost(Cost(0))
    assert(result.right.get._2 == Stream((Map.empty, 2)))
  }

  it should "fail all branches when using `_error[F].raiseError`" in {
    val a: F[Int] = 1.pure[F]
    val b: F[Int] = 2.pure[F] >> _error[F].raiseError[Int](OutOfPhlogistonsError)
    val c: F[Int] = 3.pure[F]

    val result = combineK(List(a, b, c)).runWithCost(Cost(0))
    assert(result == Left(OutOfPhlogistonsError))
  }

  it should "charge for each branch as long as `charge` is before a short-circuit" in {
    val a: F[Unit] = charge[F](Cost(1))

    val b: F[Unit] = charge[F](Cost(2)) >> A.guard(false)
    val c: F[Unit] = A.guard(false) >> charge[F](Cost(4))

    val d: F[Unit] = charge[F](Cost(8)) >> _short[F].raiseError[Unit](())
    val e: F[Unit] = _short[F].raiseError[Int](()) >> charge[F](Cost(16))

    val result = combineK(List(a, b, c, d, e)).runWithCost(Cost(1 + 2 + 8))
    assert(result == Right((Cost(0), Stream((Map.empty, ())))))
  }

}
