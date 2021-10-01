package coop.rchain.casper.state.instances

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.casperbuffer.CasperBufferKeyValueStorage.CasperBufferStorage
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.dag.state.BlockDagBufferState
import coop.rchain.casper.state.BlockStateManager
import coop.rchain.models.BlockHash.BlockHash

object BlockStateManagerImpl {
  def apply[F[_]: Sync](
      blockStore: BlockStore[F],
      blockDagStorage: BlockDagStorage[F],
      casperBufferStorage: CasperBufferStorage[F]
  ): BlockStateManager[F] =
    BlockStateManagerImpl[F](blockStore, blockDagStorage, casperBufferStorage)

  private final case class BlockStateManagerImpl[F[_]: Sync](
      blockStore: BlockStore[F],
      blockDagStorage: BlockDagStorage[F],
      casperBufferStorage: CasperBufferStorage[F]
  ) extends BlockStateManager[F] {

    override def isEmpty: F[Boolean] =
      for {
        dag       <- blockDagStorage.getRepresentation
        firstHash <- dag.topoSort(0, 1L.some)
      } yield firstHash.isEmpty

    override def casperBuffer: CasperBufferStorage[F] = casperBufferStorage
  }
}
