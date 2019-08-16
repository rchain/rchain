package coop.rchain.rholang.interpreter.accounting

import cats.Monad
import cats.data._
import cats.effect.Concurrent
import cats.effect.concurrent._
import cats.implicits._
import cats.mtl._

object CostAccounting {

  private[this] def of[F[_]: Concurrent](init: Cost): F[MonadState[F, Cost]] =
    Ref[F]
      .of(init)
      .map(defaultMonadState)

  private[this] def empty[F[_]: Concurrent]: F[MonadState[F, Cost]] =
    Ref[F]
      .of(Cost(0, "init"))
      .map(defaultMonadState)

  def emptyCost[F[_]: Concurrent](implicit L: FunctorTell[F, Chain[Cost]]): F[_cost[F]] =
    for {
      s <- Semaphore(1)
      c <- empty
    } yield (loggingCost(c, L, s))

  def initialCost[F[_]: Concurrent](
      init: Cost
  )(implicit L: FunctorTell[F, Chain[Cost]]): F[_cost[F]] =
    for {
      s <- Semaphore(1)
      c <- of(init)
    } yield (loggingCost(c, L, s))

  private[this] def defaultMonadState[F[_]: Monad: Concurrent] =
    (state: Ref[F, Cost]) =>
      new DefaultMonadState[F, Cost] {
        val monad: cats.Monad[F]  = implicitly[Monad[F]]
        def get: F[Cost]          = state.get
        def set(s: Cost): F[Unit] = state.set(s)

        override def modify(f: Cost => Cost): F[Unit] = state.modify(f.map((_, ())))
      }
}
