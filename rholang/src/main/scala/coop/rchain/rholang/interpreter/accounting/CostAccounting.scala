package coop.rchain.rholang.interpreter.accounting

import cats.data._
import cats.effect.Concurrent
import cats.effect.concurrent._
import cats.implicits._
import cats.mtl._
import coop.rchain.rholang.interpreter.{defaultMonadState, of}

object CostAccounting {

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

}
