package coop.rchain.rholang.interpreter

import cats._
import cats.data._
import cats.effect.Sync
import cats.effect.concurrent.Semaphore
import cats.syntax.all._
import cats.mtl._

import coop.rchain.catscontrib.ski.kp
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError

package object accounting extends Costs {

  type _cost[F[_]] = Semaphore[F] with MonadState[F, Cost] with FunctorTell[F, Chain[Cost]]

  def _cost[F[_]](implicit ev: _cost[F]): _cost[F] = ev

  def loggingCost[F[_]: Monad](
      state: MonadState[F, Cost],
      fTell: FunctorTell[F, Chain[Cost]],
      semaphore: Semaphore[F]
  ): _cost[F] = new Semaphore[F] with MonadState[F, Cost] with FunctorTell[F, Chain[Cost]] {
    override val functor: Functor[F]                   = Functor[F]
    override def tell(l: Chain[Cost]): F[Unit]         = fTell.tell(l)
    override def tuple[A](ta: (Chain[Cost], A)): F[A]  = fTell.tuple(ta)
    override def writer[A](a: A, l: Chain[Cost]): F[A] = fTell.writer(a, l)

    override val monad: Monad[F]                  = Monad[F]
    override def get: F[Cost]                     = state.get
    override def inspect[A](f: Cost => A): F[A]   = state.inspect(f)
    override def modify(f: Cost => Cost): F[Unit] = state.modify(f)
    override def set(s: Cost): F[Unit]            = state.set(s)

    override def acquireN(n: Long): F[Unit]       = semaphore.acquireN(n)
    override def available: F[Long]               = semaphore.available
    override def count: F[Long]                   = semaphore.count
    override def releaseN(n: Long): F[Unit]       = semaphore.releaseN(n)
    override def tryAcquireN(n: Long): F[Boolean] = semaphore.tryAcquireN(n)
    override def withPermit[A](t: F[A]): F[A]     = semaphore.withPermit(t)

  }

  def charge[F[_]: Monad](
      amount: Cost
  )(implicit cost: _cost[F], error: _error[F]): F[Unit] =
    cost.withPermit(
      cost.get.flatMap { c =>
        if (c.value < 0) error.raiseError[Unit](OutOfPhlogistonsError)
        else cost.tell(Chain.one(amount)) >> cost.set(c - amount)
      }
    ) >> error.ensure(cost.get)(OutOfPhlogistonsError)(_.value >= 0).void

  // TODO: Remove global (dummy) implicit!
  implicit def noOpCostLog[M[_]: Applicative]: FunctorTell[M, Chain[Cost]] =
    new DefaultFunctorTell[M, Chain[Cost]] {
      override val functor: Functor[M]  = implicitly[Functor[M]]
      def tell(l: Chain[Cost]): M[Unit] = Applicative[M].pure(())
    }

  implicit def ntCostLog[F[_]: Monad, G[_]: Sync](
      nt: F ~> G
  )(implicit C: _cost[F]): _cost[G] =
    new Semaphore[G] with MonadState[G, Cost] with FunctorTell[G, Chain[Cost]] {
      override val functor: Functor[G]                  = Functor[G]
      override def tell(l: Chain[Cost]): G[Unit]        = nt(C.tell(l))
      override def tuple[A](ta: (Chain[Cost], A)): G[A] = nt(C.tuple(ta))

      override val monad: Monad[G]                       = Monad[G]
      override def writer[A](a: A, l: Chain[Cost]): G[A] = nt(C.writer(a, l))
      override def get: G[Cost]                          = nt(C.get)
      override def inspect[A](f: Cost => A): G[A]        = nt(C.inspect(f))
      override def modify(f: Cost => Cost): G[Unit]      = nt(C.modify(f))
      override def set(s: Cost): G[Unit]                 = nt(C.set(s))

      override def acquireN(n: Long): G[Unit]       = nt(C.acquireN(n))
      override def available: G[Long]               = nt(C.available)
      override def count: G[Long]                   = nt(C.count)
      override def releaseN(n: Long): G[Unit]       = nt(C.releaseN(n))
      override def tryAcquireN(n: Long): G[Boolean] = nt(C.tryAcquireN(n))
      override def withPermit[A](t: G[A]): G[A] =
        Sync[G].bracket[Unit, A](acquire)(kp(t))(kp(release))
    }

}
