package coop.rchain.casper.state

import coop.rchain.blockstorage.casperbuffer.CasperBufferKeyValueStorage.CasperBufferStorage
import coop.rchain.state.StateManager

trait BlockStateManager[F[_]] extends StateManager[F] {
  def casperBuffer: CasperBufferStorage[F]
}

final case class BlockStateStatus()

object BlockStateManager {
  def apply[F[_]](implicit instance: BlockStateManager[F]): BlockStateManager[F] = instance
}
