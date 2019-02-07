package coop.rchain.blockstorage

import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockDagRepresentation.Validator
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.blockstorage.StorageError.StorageErr
import coop.rchain.casper.protocol.BlockMessage

trait BlockDagStorage[F[_]] {
  def getRepresentation: F[BlockDagRepresentation[F]]
  def insert(block: BlockMessage): F[StorageErr[BlockDagRepresentation[F]]]
  def checkpoint(): F[StorageErr[Unit]]
  def clear(): F[StorageErr[Unit]]
  def close(): F[StorageErr[Unit]]
}

object BlockDagStorage {
  def apply[F[_]](implicit B: BlockDagStorage[F]): BlockDagStorage[F] = B
}

trait BlockDagRepresentation[F[_]] {
  def children(blockHash: BlockHash): F[Option[Set[BlockHash]]]
  def lookup(blockHash: BlockHash): F[Option[BlockMetadata]]
  def contains(blockHash: BlockHash): F[Boolean]
  def topoSort(startBlockNumber: Long): F[StorageErr[Vector[Vector[BlockHash]]]]
  def topoSortTail(tailLength: Int): F[StorageErr[Vector[Vector[BlockHash]]]]
  def deriveOrdering(startBlockNumber: Long): F[StorageErr[Ordering[BlockMetadata]]]
  def latestMessageHash(validator: Validator): F[Option[BlockHash]]
  def latestMessage(validator: Validator): F[Option[BlockMetadata]]
  def latestMessageHashes: F[Map[Validator, BlockHash]]
  def latestMessages: F[Map[Validator, BlockMetadata]]
}

object BlockDagRepresentation {
  type Validator = ByteString
}
