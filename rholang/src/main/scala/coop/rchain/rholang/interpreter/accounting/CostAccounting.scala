package coop.rchain.rholang.interpreter.accounting

import cats.data._
import cats.effect.Async
import cats.syntax.all._
import cats.mtl._
import cats.Monad

import coop.rchain.metrics.{Metrics, MetricsSemaphore}
import cats.effect.Ref

object CostAccounting {

  private[this] def of[F[_]: Async](init: Cost): F[MonadState[F, Cost]] =
    Ref[F]
      .of(init)
      .map(defaultMonadState)

  private[this] def empty[F[_]: Async]: F[MonadState[F, Cost]] =
    Ref[F]
      .of(Cost(0, "init"))
      .map(defaultMonadState)

  def emptyCost[F[_]: Async: Metrics](
      implicit L: FunctorTell[F, Chain[Cost]],
      ms: Metrics.Source
  ): F[_cost[F]] =
    for {
      s <- MetricsSemaphore.single
      c <- empty
    } yield (loggingCost(c, L, s))

  def initialCost[F[_]: Async: Metrics](
      init: Cost
  )(implicit L: FunctorTell[F, Chain[Cost]], ms: Metrics.Source): F[_cost[F]] =
    for {
      s <- MetricsSemaphore.single
      c <- of(init)
    } yield (loggingCost(c, L, s))

  private[this] def defaultMonadState[F[_]: Monad: Async] =
    (state: Ref[F, Cost]) =>
      new DefaultMonadState[F, Cost] {
        val monad: cats.Monad[F]  = implicitly[Monad[F]]
        def get: F[Cost]          = state.get
        def set(s: Cost): F[Unit] = state.set(s)

        override def modify(f: Cost => Cost): F[Unit] = state.modify(f.map((_, ())))
      }
}
