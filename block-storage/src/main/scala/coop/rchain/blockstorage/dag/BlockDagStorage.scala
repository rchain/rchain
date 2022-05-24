package coop.rchain.blockstorage.dag

import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.casper.protocol.{BlockMessage, DeployData}
import coop.rchain.crypto.signatures.Signed
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata

trait BlockDagStorage[F[_]] {
  def getRepresentation: F[DagRepresentation]
  def insert(
      block: BlockMessage,
      invalid: Boolean,
      approved: Boolean = false
  ): F[DagRepresentation]
  def recordDirectlyFinalized(
      direct: BlockHash,
      finalizationEffect: Set[BlockHash] => F[Unit]
  ): F[Unit]
  def lookup(blockHash: BlockHash): F[Option[BlockMetadata]]
  def lookupByDeployId(blockHash: DeployId): F[Option[BlockHash]]
  def addDeploy(d: Signed[DeployData]): F[Unit]
  def pooledDeploys: F[Map[DeployId, Signed[DeployData]]]
  def deployStatus(d: DeployId): F[String]
}

object BlockDagStorage {
  type DeployId = ByteString

  def apply[F[_]](implicit instance: BlockDagStorage[F]): BlockDagStorage[F] = instance
}
