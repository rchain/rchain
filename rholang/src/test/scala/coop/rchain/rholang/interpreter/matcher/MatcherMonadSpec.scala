package coop.rchain.rholang.interpreter.matcher

import cats._
import cats.data.{State, StateT}
import cats.effect._
import cats.effect.unsafe.implicits.global
import cats.instances.list._
import cats.mtl.implicits._
import cats.syntax.all._
import coop.rchain.metrics.Metrics
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting.CostAccounting.{CostState, CostStateRef}
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.matcher.{run => runMatcher}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

object MatcherMonadHelper {
  type CostStateMonad[F[_]] = CostStateRef[F] with Monad[F]

  implicit def ntCostLog[F[_]: Monad: CostStateRef, G[_]: Sync](
      nt: F ~> G
  ): CostStateMonad[G] = {
    val C = CostStateRef[F]
    val M = Monad[G]
    new Ref[G, CostState] with Monad[G] {
      override def get: G[CostState]                     = nt(C.get)
      override def set(a: CostState): G[Unit]            = nt(C.set(a))
      override def getAndSet(a: CostState): G[CostState] = nt(C.getAndSet(a))
      override def access: G[(CostState, CostState => G[Boolean])] =
        nt(C.access.map { case (s, f) => (s, x => nt(f(x))) })
      override def tryUpdate(f: CostState => CostState): G[Boolean]           = nt(C.tryUpdate(f))
      override def tryModify[B](f: CostState => (CostState, B)): G[Option[B]] = nt(C.tryModify(f))
      override def update(f: CostState => CostState): G[Unit]                 = nt(C.update(f))
      override def modify[B](f: CostState => (CostState, B)): G[B]            = nt(C.modify(f))
      override def tryModifyState[B](state: State[CostState, B]): G[Option[B]] =
        nt(C.tryModifyState(state))
      override def modifyState[B](state: State[CostState, B]): G[B] = nt(C.modifyState(state))

      override def pure[A](x: A): G[A]                                 = M.pure(x)
      override def flatMap[A, B](fa: G[A])(f: A => G[B]): G[B]         = M.flatMap(fa)(f)
      override def tailRecM[A, B](a: A)(f: A => G[Either[A, B]]): G[B] = M.tailRecM(a)(f)
    }
  }

  def matcherMonadCostLog[F[_]: Sync: CostStateRef](): CostStateMonad[MatcherMonadT[F, *]] =
    λ[F ~> MatcherMonadT[F, *]](fa => StateT.liftF(StreamT.liftF(fa)))
}

class MatcherMonadSpec extends AnyFlatSpec with Matchers {
  import MatcherMonadHelper._
  implicit val metrics: Metrics[IO] = new Metrics.MetricsNOP[IO]
  implicit val ms: Metrics.Source   = Metrics.BaseSource

  type F[A] = MatcherMonadT[IO, A]

  import StreamT.streamTSync

  val A: Alternative[F] = Alternative[F]

  implicit val cost = CostAccounting.emptyCost[IO].unsafeRunSync()

  implicit val costF: CostStateRef[F] = matcherMonadCostLog[IO]()
  implicit val matcherMonadError      = implicitly[Sync[F]]

  private def combineK[FF[_]: MonoidK, G[_]: Foldable, A](gfa: G[FF[A]]): FF[A] =
    gfa.foldLeft(MonoidK[FF].empty[A])(SemigroupK[FF].combineK[A])

  private def runWithCost[A](f: IO[A], phlo: Int) =
    (for {
      _        <- cost.set(Cost(phlo, "initial cost"))
      result   <- f
      phloLeft <- cost.get
    } yield (phloLeft, result)).unsafeRunSync()

  behavior of "MatcherMonad"

  it should "charge for each non-deterministic branch" in {
    val possibleResults = LazyList((0, 1), (0, 2))
    val computation     = A.unite(possibleResults.pure[F])
    val sum             = computation.map { case (x, y) => x + y } >>= (charge[F](Cost(1)).as(_))
    val (phloLeft, _)   = runWithCost(runMatcher(sum), possibleResults.size)
    assert(phloLeft.total.value == 0)

    val moreVariants    = sum.flatMap(x => A.unite(LazyList(x, 0, -x).pure[F]))
    val moreComputation = moreVariants.map(x => "Do sth with " + x) >>= (charge[F](Cost(1)).as(_))
    val (phloLeft2, _) =
      runWithCost(runMatcher(moreComputation), possibleResults.size * 3 + possibleResults.size)
    assert(phloLeft2.total.value == 0)

  }

  val modifyStates = for {
    _ <- _freeMap[F].set(Map(42 -> Par()))
    _ <- costF <+ Cost(1)
  } yield ()

  it should "retain cost and matches when attemptOpt is called on successful match" in {
    val (phloLeft, res) = runWithCost(runFirst(attemptOpt[F, Unit](modifyStates)), 0)
    assert(phloLeft.total.value == 1)
    assert(res == Some((Map(42 -> Par()), Some(()))))
  }

  it should "retain cost but discard matches when attemptOpt is called on a match failed using _short" in {
    val failed = for {
      _ <- modifyStates
      _ <- _short[F].raiseError[Int](())
    } yield ()

    val (phloLeft, res) = runWithCost(runFirst(attemptOpt[F, Unit](failed)), 0)
    assert(phloLeft.total.value == 1)
    assert(res == Some((Map.empty, None)))

  }

  it should "retain cost but discard matches when attemptOpt is called on a match failed using `guard`" in {
    val failed = for {
      _ <- modifyStates
      _ <- A.guard(false)
    } yield ()

    val (phloLeft, res) = runWithCost(runFirst(attemptOpt[F, Unit](failed)), 0)
    assert(phloLeft.total.value == 1)
    assert(res == Some((Map.empty, None)))
  }

  it should "apply `guard`-s separately to each computation branch" in {
    val a: F[Int] = A.guard(true) >> 1.pure[F]
    val b: F[Int] = A.guard(false) >> 2.pure[F]
    val c: F[Int] = A.guard(true) >> 3.pure[F]
    val combined  = combineK(List(a, b, c))

    val (phloLeft, res) = runWithCost(runMatcher(combined), 0)
    assert(phloLeft.total.value == 0)
    assert(res == LazyList((Map.empty, 1), (Map.empty, 3)))
  }

  it should "apply `_short[F].raiseError` separately to each computation branch" in {
    val a: F[Int] = _short[F].raiseError[Int](()) >> 1.pure[F]
    val b: F[Int] = 2.pure[F]
    val c: F[Int] = 3.pure[F] >> _short[F].raiseError[Int](())
    val combined  = combineK(List(a, b, c))

    val (phloLeft, res) = runWithCost(runMatcher(combined), 0)
    assert(phloLeft.total.value == 0)
    assert(res == LazyList((Map.empty, 2)))
  }

  it should "fail all branches when using `_error[F].raise`" in {
    val a: F[Int] = 1.pure[F]
    val b: F[Int] = 2.pure[F] >> Sync[F].raiseError[Int](OutOfPhlogistonsError)
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

    assert(phloLeft.total.value == 0)
    assert(res == LazyList((Map.empty, ())))

  }

}
