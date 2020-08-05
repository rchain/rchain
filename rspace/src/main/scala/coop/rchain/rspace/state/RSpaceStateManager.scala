package coop.rchain.rspace.state

import coop.rchain.state.StateManager

trait RSpaceStateManager[F[_]] extends StateManager[F] {
  def exporter: RSpaceExporter[F]
  def importer: RSpaceImporter[F]
}

object RSpaceStateManager {
  def apply[F[_]](implicit instance: RSpaceStateManager[F]): RSpaceStateManager[F] = instance
}
