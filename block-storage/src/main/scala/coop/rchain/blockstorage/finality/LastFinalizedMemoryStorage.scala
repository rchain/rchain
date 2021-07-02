package coop.rchain.blockstorage.finality

import cats.Functor
import cats.effect.Concurrent
import cats.syntax.functor._
import cats.syntax.option._
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.Cell

class LastFinalizedMemoryStorage[F[_]: Functor](
    lastFinalizedBlockHashState: Cell[F, Option[BlockHash]]
) extends LastFinalizedStorage[F] {
  override def put(blockHash: BlockHash): F[Unit] =
    lastFinalizedBlockHashState.set(blockHash.some)

  override def get(): F[Option[BlockHash]] =
    lastFinalizedBlockHashState.read
}

object LastFinalizedMemoryStorage {
  def make[F[_]: Concurrent]: F[LastFinalizedStorage[F]] =
    for {
      state <- Cell.mvarCell[F, Option[BlockHash]](none[BlockHash])
    } yield new LastFinalizedMemoryStorage[F](state)
}
