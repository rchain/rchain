package coop.rchain.blockstorage

import cats.Applicative
import cats.implicits._
import coop.rchain.casper.protocol.{ApprovedBlock, BlockMessage}
import coop.rchain.models.BlockHash.BlockHash

import scala.language.higherKinds

trait BlockStore[F[_]] {
  def put(blockMessage: BlockMessage): F[Unit] =
    put((blockMessage.blockHash, blockMessage))

  def put(blockHash: BlockHash, blockMessage: BlockMessage): F[Unit] =
    put((blockHash, blockMessage))

  def get(blockHash: BlockHash): F[Option[BlockMessage]]

  def find(p: BlockHash => Boolean): F[Seq[(BlockHash, BlockMessage)]]

  def put(f: => (BlockHash, BlockMessage)): F[Unit]

  def apply(blockHash: BlockHash)(implicit applicativeF: Applicative[F]): F[BlockMessage] =
    get(blockHash).map(_.get)

  def contains(blockHash: BlockHash)(implicit applicativeF: Applicative[F]): F[Boolean] =
    get(blockHash).map(_.isDefined)

  def getApprovedBlock: F[Option[ApprovedBlock]]

  def putApprovedBlock(block: ApprovedBlock): F[Unit]

  def checkpoint(): F[Unit]

  def clear(): F[Unit]

  def close(): F[Unit]
}

object BlockStore {

  def apply[F[_]](implicit ev: BlockStore[F]): BlockStore[F] = ev
}
