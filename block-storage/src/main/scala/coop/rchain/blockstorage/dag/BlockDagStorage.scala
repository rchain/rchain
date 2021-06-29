package coop.rchain.blockstorage.dag

import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.{BlockMetadata, EquivocationRecord}

trait BlockDagStorage[F[_]] {
  def getRepresentation: F[BlockDagRepresentation[F]]
  def insert(
      block: BlockMessage,
      invalid: Boolean,
      approved: Boolean = false
  ): F[BlockDagRepresentation[F]]
  def accessEquivocationsTracker[A](f: EquivocationsTracker[F] => F[A]): F[A]
  def recordDirectlyFinalised(blockHash: BlockHash): F[Unit]
  def addFinalizedBlockHash(blockHash: BlockHash): F[Unit]
}

object BlockDagStorage {
  type DeployId = ByteString

  def apply[F[_]](implicit instance: BlockDagStorage[F]): BlockDagStorage[F] = instance
}

trait BlockDagRepresentation[F[_]] {
  def children(blockHash: BlockHash): F[Option[Set[BlockHash]]]
  def lookup(blockHash: BlockHash): F[Option[BlockMetadata]]
  def contains(blockHash: BlockHash): F[Boolean]
  def latestMessageHash(validator: Validator): F[Option[BlockHash]]
  def latestMessageHashes: F[Map[Validator, BlockHash]]
  def invalidBlocks: F[Set[BlockMetadata]]
  // For BlockAPI
  def latestBlockNumber: F[Long]
  def lookupByDeployId(deployId: DeployId): F[Option[BlockHash]]
  def topoSort(
      startBlockNumber: Long,
      maybeEndBlockNumber: Option[Long]
  ): F[Vector[Vector[BlockHash]]]
  // DAG representation has to have finalized block, or it does not make sense
  def lastFinalizedBlock: BlockHash
  def isFinalized(blockHash: BlockHash): F[Boolean]
}

trait EquivocationsTracker[F[_]] {
  def equivocationRecords: F[Set[EquivocationRecord]]
  def insertEquivocationRecord(record: EquivocationRecord): F[Unit]
  def updateEquivocationRecord(record: EquivocationRecord, blockHash: BlockHash): F[Unit]
}
