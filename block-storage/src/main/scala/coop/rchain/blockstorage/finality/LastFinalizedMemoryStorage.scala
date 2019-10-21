package coop.rchain.blockstorage.finality

import cats.Functor
import cats.syntax.functor._
import cats.syntax.option._
import cats.effect.Concurrent
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.Cell

class LastFinalizedMemoryStorage[F[_]: Functor](
    lastFinalizedBlockHashState: Cell[F, Option[BlockHash]]
) extends LastFinalizedStorage[F] {
  override def put(blockHash: BlockHash): F[Unit] =
    lastFinalizedBlockHashState.set(blockHash.some)

  override def get(genesis: BlockMessage): F[BlockHash] =
    lastFinalizedBlockHashState.read.map(_.getOrElse(genesis.blockHash))
}

object LastFinalizedMemoryStorage {
  def make[F[_]: Concurrent]: F[LastFinalizedStorage[F]] =
    for {
      state <- Cell.mvarCell[F, Option[BlockHash]](none[BlockHash])
    } yield new LastFinalizedMemoryStorage[F](state)
}
