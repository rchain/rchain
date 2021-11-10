package coop.rchain.blockstorage

import cats.Applicative
import cats.syntax.all._
import coop.rchain.casper.protocol.{ApprovedBlock, BlockMessage}
import coop.rchain.models.BlockHash.BlockHash

trait BlockStore[F[_]] {
  def get(blockHash: BlockHash): F[Option[BlockMessage]]

  def put(f: => (BlockHash, BlockMessage)): F[Unit]

  def contains(blockHash: BlockHash)(implicit applicativeF: Applicative[F]): F[Boolean] =
    get(blockHash).map(_.isDefined)

  def getApprovedBlock: F[Option[ApprovedBlock]]

  def putApprovedBlock(block: ApprovedBlock): F[Unit]

  // Defaults

  def put(blockMessage: BlockMessage): F[Unit] =
    put((blockMessage.blockHash, blockMessage))

  def put(blockHash: BlockHash, blockMessage: BlockMessage): F[Unit] =
    put((blockHash, blockMessage))
}

object BlockStore {
  def apply[F[_]](implicit instance: BlockStore[F]): BlockStore[F] = instance
}
