package coop.rchain.rholang.interpreter

import cats.Monad
import cats.implicits._
import cats.mtl.MonadState

import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError

package object accounting extends Costs {

  type _cost[F[_]] = MonadState[F, Cost]
  def _cost[F[_]](implicit ev: _cost[F]): _cost[F] = ev

  def charge[F[_]: Monad](
      amount: Cost
  )(implicit cost: _cost[F], error: _error[F]): F[Unit] =
    for {
      _ <- error.ensure(cost.get)(OutOfPhlogistonsError)(_.value >= 0)
      _ <- cost.modify(_ - amount)
      _ <- error.ensure(cost.get)(OutOfPhlogistonsError)(_.value >= 0)
    } yield ()

}
