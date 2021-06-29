package coop.rchain.blockstorage.finality

import java.nio.file.Path
import cats.syntax.all._
import cats.effect.{Concurrent, Sync}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.codecs.{codecBlockHash, codecSeqNum}
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.Log
import coop.rchain.store.{KeyValueStore, KeyValueTypedStore}
import coop.rchain.shared.syntax._
import coop.rchain.blockstorage.syntax._

class LastFinalizedKeyValueStorage[F[_]: Sync] private (
    lastFinalizedBlockDb: KeyValueTypedStore[F, Int, BlockHash]
) extends LastFinalizedStorage[F] {
  val fixedKey = 1
  override def put(blockHash: BlockHash): F[Unit] =
    lastFinalizedBlockDb.put(Seq((fixedKey, blockHash)))

  override def get(): F[Option[BlockHash]] =
    lastFinalizedBlockDb.get(fixedKey)

  val DONE = ByteString.copyFrom(Array.fill[Byte](32)(-1))

  override def requireMigration: F[Boolean] =
    get().map(_.exists(_ != DONE))

  override def recordMigrationDone: F[Unit] = put(DONE)
}

object LastFinalizedKeyValueStorage {
  def apply[F[_]: Sync](
      lastFinalizedBlockDb: KeyValueTypedStore[F, Int, BlockHash]
  ): LastFinalizedKeyValueStorage[F] = new LastFinalizedKeyValueStorage(lastFinalizedBlockDb)

  def apply[F[_]: Sync](keyValueStore: KeyValueStore[F]): LastFinalizedKeyValueStorage[F] =
    apply(keyValueStore.toTypedStore(codecSeqNum, codecBlockHash))
}
