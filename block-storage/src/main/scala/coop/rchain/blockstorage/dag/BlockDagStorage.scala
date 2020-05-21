package coop.rchain.blockstorage.dag

import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.casper.protocol.{BlockMessage, BlockMessageProto}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.{BlockMetadata, EquivocationRecord}

trait BlockDagStorage[F[_]] {
  def getRepresentation: F[BlockDagRepresentation[F]]
  def insert(
      block: BlockMessage,
      genesis: BlockMessage,
      invalid: Boolean
  ): F[BlockDagRepresentation[F]]
  def accessEquivocationsTracker[A](f: EquivocationsTracker[F] => F[A]): F[A]
  def checkpoint(): F[Unit]
  def close(): F[Unit]
  def recordSlasedValidator(validator: Validator): F[Unit]
}

object BlockDagStorage {
  type DeployId = ByteString

  def apply[F[_]](implicit B: BlockDagStorage[F]): BlockDagStorage[F] = B
}

trait BlockDagRepresentation[F[_]] {
  def children(blockHash: BlockHash): F[Option[Set[BlockHash]]]
  def lookup(blockHash: BlockHash): F[Option[BlockMetadata]]
  def contains(blockHash: BlockHash): F[Boolean]
  def lookupByDeployId(deployId: DeployId): F[Option[BlockHash]]
  def topoSort(
      startBlockNumber: Long,
      maybeEndBlockNumber: Option[Long]
  ): F[Vector[Vector[BlockHash]]]
  def latestBlockNumber: F[Long]
  def latestMessageHash(validator: Validator): F[Option[BlockHash]]
  def latestMessage(validator: Validator): F[Option[BlockMetadata]]
  def latestMessageHashes: F[Map[Validator, BlockHash]]
  def latestMessages: F[Map[Validator, BlockMetadata]]
  def invalidBlocks: F[Set[BlockMetadata]]
  def slashedInvalidValidators: F[Set[Validator]]
}

trait EquivocationsTracker[F[_]] {
  def equivocationRecords: F[Set[EquivocationRecord]]
  def insertEquivocationRecord(record: EquivocationRecord): F[Unit]
  def updateEquivocationRecord(record: EquivocationRecord, blockHash: BlockHash): F[Unit]
}
