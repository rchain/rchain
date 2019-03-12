package coop.rchain.blockstorage

import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockDagRepresentation.Validator
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.models.{BlockMetadata, EquivocationRecord}

trait BlockDagStorage[F[_]] {
  def getRepresentation: F[BlockDagRepresentation[F]]
  def insert(block: BlockMessage, invalid: Boolean): F[BlockDagRepresentation[F]]
  def accessEquivocationsTracker[A](f: EquivocationsTracker[F] => F[A]): F[A]
  def checkpoint(): F[Unit]
  def clear(): F[Unit]
  def close(): F[Unit]
}

object BlockDagStorage {
  def apply[F[_]](implicit B: BlockDagStorage[F]): BlockDagStorage[F] = B
}

trait BlockDagRepresentation[F[_]] {
  def children(blockHash: BlockHash): F[Option[Set[BlockHash]]]
  def lookup(blockHash: BlockHash): F[Option[BlockMetadata]]
  def unsafeLookup(blockHash: BlockHash): F[BlockMetadata]
  def contains(blockHash: BlockHash): F[Boolean]
  def topoSort(startBlockNumber: Long): F[Vector[Vector[BlockHash]]]
  def topoSortTail(tailLength: Int): F[Vector[Vector[BlockHash]]]
  def deriveOrdering(startBlockNumber: Long): F[Ordering[BlockMetadata]]
  def latestMessageHash(validator: Validator): F[Option[BlockHash]]
  def latestMessage(validator: Validator): F[Option[BlockMetadata]]
  def latestMessageHashes: F[Map[Validator, BlockHash]]
  def latestMessages: F[Map[Validator, BlockMetadata]]
}

object BlockDagRepresentation {
  type Validator = ByteString
}

trait EquivocationsTracker[F[_]] {
  def equivocationRecords: F[Set[EquivocationRecord]]
  def insertEquivocationRecord(record: EquivocationRecord): F[Unit]
  def updateEquivocationRecord(record: EquivocationRecord, blockHash: BlockHash): F[Unit]
}
