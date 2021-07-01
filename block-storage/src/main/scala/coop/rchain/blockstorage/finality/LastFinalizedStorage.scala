package coop.rchain.blockstorage.finality

import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.models.BlockHash.BlockHash

trait LastFinalizedStorage[F[_]] {
  def put(blockHash: BlockHash): F[Unit]
  def get(): F[Option[BlockHash]]
}

object LastFinalizedStorage {
  def apply[F[_]](implicit ev: LastFinalizedStorage[F]): LastFinalizedStorage[F] = ev
}
