package coop.rchain.blockstorage.finality

import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.models.BlockHash.BlockHash

trait LastFinalizedStorage[F[_]] {
  def put(blockHash: BlockHash): F[Unit]
  def get(): F[Option[BlockHash]]

  /** If LFB is migrated to BlockDagStorage metadata */
  def requireMigration: F[Boolean]
  def recordMigrationDone: F[Unit]
}

object LastFinalizedStorage {
  def apply[F[_]](implicit ev: LastFinalizedStorage[F]): LastFinalizedStorage[F] = ev
}
