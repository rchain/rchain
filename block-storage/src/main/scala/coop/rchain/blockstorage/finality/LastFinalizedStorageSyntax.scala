package coop.rchain.blockstorage.finality

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.models.BlockHash.BlockHash

trait LastFinalizedStorageSyntax {
  implicit final def blockStorageSyntaxLastFinalizedStorage[F[_]: Sync](
      store: LastFinalizedStorage[F]
  ): LastFinalizedStorageOps[F] = new LastFinalizedStorageOps[F](store)
}

object LastFinalizedBlockNotFound extends Exception

final class LastFinalizedStorageOps[F[_]: Sync](
    private val store: LastFinalizedStorage[F]
) {
  def getOrElse(blockHash: BlockHash): F[BlockHash] = store.get().map(_.getOrElse(blockHash))

  def getUnSafe: F[BlockHash] = store.get() >>= (_.liftTo(LastFinalizedBlockNotFound))
}
