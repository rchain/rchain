package coop.rchain.blockstorage.dag

import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.casper.protocol.{BlockMessage, DeployData}
import coop.rchain.crypto.signatures.Signed
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata

trait BlockDagStorage[F[_]] {

  def getRepresentation: F[DagRepresentation]

  def insertNew(blockMetadata: BlockMetadata, block: BlockMessage): F[DagRepresentation]

  // TODO: legacy function, used only in tests, it should be removed when tests are fixed
  def insert(block: BlockMessage, invalid: Boolean, approved: Boolean = false): F[DagRepresentation]

  def lookup(blockHash: BlockHash): F[Option[BlockMetadata]]

  /* Deploys included in the DAG */

  def lookupByDeployId(blockHash: DeployId): F[Option[BlockHash]]

  /* Deploy pool, not processed (finalized) deploys */

  def addDeploy(d: Signed[DeployData]): F[Unit]
  def pooledDeploys: F[Map[DeployId, Signed[DeployData]]]
  def containsDeployInPool(deployId: DeployId): F[Boolean]
}

object BlockDagStorage {
  type DeployId = ByteString

  def apply[F[_]](implicit instance: BlockDagStorage[F]): BlockDagStorage[F] = instance
}
