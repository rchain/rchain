package coop.rchain.rholang.interpreter.accounting

import cats.effect.Concurrent
import cats.effect.concurrent._
import cats.implicits._
import cats.mtl._
import cats.{FlatMap, Monad}
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError

object CostAccounting {

  def of[F[_]: Concurrent](init: Cost): F[MonadState[F, Cost]] =
    Ref[F]
      .of(init)
      .map(defaultMonadState)

  def empty[F[_]: Concurrent]: F[MonadState[F, Cost]] =
    Ref[F]
      .of(Cost(0, "init"))
      .map(defaultMonadState)

  def emptyCost[F[_]: Concurrent]: F[_cost[F]] =
    for {
      s <- Semaphore(1)
      c <- empty
    } yield (loggingCost(c, noOpCostLog, s))

  def initialCost[F[_]: Concurrent](init: Cost): F[_cost[F]] =
    for {
      s <- Semaphore(1)
      c <- of(init)
    } yield (loggingCost(c, noOpCostLog, s))

  private[this] def defaultMonadState[F[_]: Monad: Concurrent] =
    (state: Ref[F, Cost]) =>
      new DefaultMonadState[F, Cost] {
        val monad: cats.Monad[F]  = implicitly[Monad[F]]
        def get: F[Cost]          = state.get
        def set(s: Cost): F[Unit] = state.set(s)

        override def modify(f: Cost => Cost): F[Unit] = state.modify(f.map((_, ())))
      }
}
