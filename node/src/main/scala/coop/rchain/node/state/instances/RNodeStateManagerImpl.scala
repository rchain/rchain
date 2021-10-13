package coop.rchain.node.state.instances

import cats.effect.Sync
import coop.rchain.blockstorage.casperbuffer.CasperBufferKeyValueStorage.CasperBufferStorage
import coop.rchain.casper.state.{BlockStateManager, RNodeStateManager}
import coop.rchain.catscontrib.Catscontrib.ToBooleanF
import coop.rchain.rspace.state.RSpaceStateManager

object RNodeStateManagerImpl {
  def apply[F[_]: Sync](
      rSpaceStateManager: RSpaceStateManager[F],
      blockStateManager: BlockStateManager[F]
  ): RNodeStateManager[F] =
    RNodeStateManagerImpl[F](rSpaceStateManager, blockStateManager)

  private final case class RNodeStateManagerImpl[F[_]: Sync](
      rSpaceStateManager: RSpaceStateManager[F],
      blockStateManager: BlockStateManager[F]
  ) extends RNodeStateManager[F] {
    override def isEmpty: F[Boolean]                         = rSpaceStateManager.isEmpty &&^ blockStateManager.isEmpty
    override def casperBufferStorage: CasperBufferStorage[F] = blockStateManager.casperBuffer
  }
}
