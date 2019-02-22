package coop.rchain.rholang.interpreter

import cats._
import cats.data._
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import cats.mtl._

import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError

package object accounting extends Costs {

  type _cost[F[_]]    = MonadState[F, Cost]
  type _costLog[F[_]] = FunctorTell[F, Chain[Cost]]

  def _cost[F[_]](implicit ev: _cost[F]): _cost[F]          = ev
  def _costLog[F[_]](implicit ev: _costLog[F]): _costLog[F] = ev

  def charge[F[_]: Monad](
      amount: Cost
  )(implicit cost: _cost[F], error: _error[F], writer: _costLog[F]): F[Unit] =
    for {
      _ <- error.ensure(cost.get)(OutOfPhlogistonsError)(_.value >= 0)
      _ <- writer.tell(Chain.one(amount))
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

  def noOpCostLog[M[_]: Applicative]: _costLog[M] =
    new DefaultFunctorTell[M, Chain[Cost]] {
      override val functor: Functor[M]  = implicitly[Functor[M]]
      def tell(l: Chain[Cost]): M[Unit] = Applicative[M].pure(())
    }

  implicit def ntCostLog[F[_]: Monad: _costLog, G[_]: Functor](
      nt: F ~> G
  ): _costLog[G] =
    new DefaultFunctorTell[G, Chain[Cost]] {
      override val functor: Functor[G]  = Functor[G]
      def tell(l: Chain[Cost]): G[Unit] = nt(_costLog[F].tell(l))
    }

}
