package coop.rchain.blockstorage

import cats.Applicative
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.BlockMessage

import scala.language.higherKinds

trait BlockStore[F[_]] {
  import BlockStore.BlockHash

  def put(blockHash: BlockHash, blockMessage: BlockMessage.BlockMessageSafe): F[Unit] =
    put((blockHash, blockMessage))

  def get(blockHash: BlockHash): F[Option[BlockMessage.BlockMessageSafe]]

  def find(p: BlockHash => Boolean): F[Seq[(BlockHash, BlockMessage.BlockMessageSafe)]]

  def put(f: => (BlockHash, BlockMessage.BlockMessageSafe)): F[Unit]

  def apply(blockHash: BlockHash)(
      implicit applicativeF: Applicative[F]): F[BlockMessage.BlockMessageSafe] =
    get(blockHash).map(_.get)

  def contains(blockHash: BlockHash)(implicit applicativeF: Applicative[F]): F[Boolean] =
    get(blockHash).map(_.isDefined)

  def asMap(): F[Map[BlockHash, BlockMessage.BlockMessageSafe]]

  def clear(): F[Unit]

  def close(): F[Unit]
}

object BlockStore {

  def apply[F[_]](implicit ev: BlockStore[F]): BlockStore[F] = ev

  type BlockHash = ByteString

}
