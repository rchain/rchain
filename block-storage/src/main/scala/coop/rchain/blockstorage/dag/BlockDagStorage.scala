package coop.rchain.blockstorage.dag

import cats.Applicative
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.dag.state.BlockDagRepresentationState
import coop.rchain.blockstorage.dag.state.BlockDagRepresentationState.BlockDagFinalizationState
import coop.rchain.casper.protocol.{BlockMessage, StateMetadata}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.{BlockMetadata, EquivocationRecord}
import cats.syntax.all._

trait BlockDagStorage[F[_]] {
  def getRepresentation: F[BlockDagRepresentation[F]]
  def insert(
      block: BlockMessage,
      invalid: Boolean,
      blockStateMetadata: StateMetadata
  ): F[BlockDagRepresentation[F]]
  def accessEquivocationsTracker[A](f: EquivocationsTracker[F] => F[A]): F[A]
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
  def isFinalized(blockHash: BlockHash): F[Boolean]
  def genesis: F[BlockHash]
  def finalizationState: BlockDagFinalizationState
  def getPureState: BlockDagRepresentationState
}

trait EquivocationsTracker[F[_]] {
  def equivocationRecords: F[Set[EquivocationRecord]]
  def insertEquivocationRecord(record: EquivocationRecord): F[Unit]
  def updateEquivocationRecord(record: EquivocationRecord, blockHash: BlockHash): F[Unit]
}
