package coop.rchain.rholang.interpreter

import cats._
import cats.data._
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import cats.mtl._

import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError

package object accounting extends Costs {

  type _cost[F[_]] = MonadState[F, Cost] with FunctorTell[F, Chain[Cost]]

  def _cost[F[_]](implicit ev: _cost[F]): _cost[F] = ev

  def loggingCost[F[_]: Monad](
      state: MonadState[F, Cost],
      fTell: FunctorTell[F, Chain[Cost]]
  ): _cost[F] =
    new MonadState[F, Cost] with FunctorTell[F, Chain[Cost]] {
      override val functor: Functor[F]                   = Functor[F]
      override def tell(l: Chain[Cost]): F[Unit]         = fTell.tell(l)
      override def tuple[A](ta: (Chain[Cost], A)): F[A]  = fTell.tuple(ta)
      override def writer[A](a: A, l: Chain[Cost]): F[A] = fTell.writer(a, l)

      override val monad: Monad[F]                  = Monad[F]
      override def get: F[Cost]                     = state.get
      override def inspect[A](f: Cost => A): F[A]   = state.inspect(f)
      override def modify(f: Cost => Cost): F[Unit] = state.modify(f)
      override def set(s: Cost): F[Unit]            = state.set(s)
    }

  def charge[F[_]: Monad](
      amount: Cost
  )(implicit cost: _cost[F], error: _error[F]): F[Unit] =
    for {
      _ <- error.ensure(cost.get)(OutOfPhlogistonsError)(_.value >= 0)
      _ <- cost.tell(Chain.one(amount))
      _ <- cost.modify(_ - amount)
      _ <- error.ensure(cost.get)(OutOfPhlogistonsError)(_.value >= 0)
    } yield ()

  def costLog[M[_]: Sync](): FunctorListen[M, Chain[Cost]] =
    new DefaultFunctorListen[M, Chain[Cost]] {
      override val functor: Functor[M]  = implicitly[Functor[M]]
      private val ref                   = Ref.unsafe(Chain.empty[Cost])
      def tell(l: Chain[Cost]): M[Unit] = ref.modify(c => (c.concat(l), ()))
      def listen[A](fa: M[A]): M[(A, Chain[Cost])] =
        for {
          a <- fa
          r <- ref.get
        } yield ((a, r))
    }

  def noOpCostLog[M[_]: Applicative]: FunctorTell[M, Chain[Cost]] =
    new DefaultFunctorTell[M, Chain[Cost]] {
      override val functor: Functor[M]  = implicitly[Functor[M]]
      def tell(l: Chain[Cost]): M[Unit] = Applicative[M].pure(())
    }

  implicit def ntCostLog[F[_]: Monad, G[_]: Monad](
      nt: F ~> G
  )(implicit C: _cost[F]): _cost[G] =
    new MonadState[G, Cost] with FunctorTell[G, Chain[Cost]] {
      override val functor: Functor[G]                  = Functor[G]
      override def tell(l: Chain[Cost]): G[Unit]        = nt(C.tell(l))
      override def tuple[A](ta: (Chain[Cost], A)): G[A] = nt(C.tuple(ta))

      override val monad: Monad[G]                       = Monad[G]
      override def writer[A](a: A, l: Chain[Cost]): G[A] = nt(C.writer(a, l))
      override def get: G[Cost]                          = nt(C.get)
      override def inspect[A](f: Cost => A): G[A]        = nt(C.inspect(f))
      override def modify(f: Cost => Cost): G[Unit]      = nt(C.modify(f))
      override def set(s: Cost): G[Unit]                 = nt(C.set(s))
    }

}
