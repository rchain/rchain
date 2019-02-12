package coop.rchain.rholang.interpreter

import cats.mtl.MonadState

package object accounting extends Costs {

  type _cost[F[_]] = MonadState[F, Cost]
  def _cost[F[_]](implicit ev: _cost[F]): _cost[F] = ev
}
