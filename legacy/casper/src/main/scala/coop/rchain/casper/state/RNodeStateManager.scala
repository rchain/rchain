package coop.rchain.casper.state

import coop.rchain.state.StateManager

trait RNodeStateManager[F[_]] extends StateManager[F]

object RNodeStateManager {
  def apply[F[_]](implicit instance: RNodeStateManager[F]): RNodeStateManager[F] = instance
}
