package coop.rchain.blockstorage.finality

import java.nio.file.Path

import cats.syntax.all._
import cats.effect.{Concurrent, Sync}
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
}

object LastFinalizedKeyValueStorage {
  def apply[F[_]: Sync](
      lastFinalizedBlockDb: KeyValueTypedStore[F, Int, BlockHash]
  ): LastFinalizedKeyValueStorage[F] = new LastFinalizedKeyValueStorage(lastFinalizedBlockDb)

  def apply[F[_]: Sync](keyValueStore: KeyValueStore[F]): LastFinalizedKeyValueStorage[F] =
    apply(keyValueStore.toTypedStore(codecSeqNum, codecBlockHash))

  def importFromFileStorage[F[_]: Concurrent: Log: Metrics](
      oldLastFinalizedPath: Path,
      lastFinalizedBlockDb: LastFinalizedKeyValueStorage[F]
  ): F[Unit] =
    for {
      _                       <- Log[F].warn(s"Starting Last Finalized Block migration, loading existing data.")
      oldLastFinalizedStorage <- LastFinalizedFileStorage.make[F](oldLastFinalizedPath)
      lastFinalizedBlock      <- oldLastFinalizedStorage.getUnSafe
      _                       <- Log[F].warn(s"Last Finalized Block is $lastFinalizedBlock .")
      _                       <- lastFinalizedBlockDb.put(lastFinalizedBlock)
      _                       <- Log[F].warn(s"Migrate Last Finalized Block done.")
    } yield ()
}
