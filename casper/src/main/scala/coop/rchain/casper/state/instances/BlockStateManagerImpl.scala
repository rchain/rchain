package coop.rchain.casper.state.instances

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper.state.BlockStateManager

object BlockStateManagerImpl {
  def apply[F[_]: Sync](
      blockStore: BlockStore[F],
      blockDagStorage: BlockDagStorage[F]
  ): BlockStateManager[F] =
    BlockStateManagerImpl[F](blockStore, blockDagStorage)

  private final case class BlockStateManagerImpl[F[_]: Sync](
      blockStore: BlockStore[F],
      blockDagStorage: BlockDagStorage[F]
  ) extends BlockStateManager[F] {

    override def isEmpty: F[Boolean] =
      for {
        dag       <- blockDagStorage.getRepresentation
        firstHash <- dag.topoSort(0, 1L.some)
      } yield firstHash.isEmpty
  }
}
