package coop.rchain.blockstorage.casperbuffer

import coop.rchain.blockstorage.util.{BlockDependencyDag, DoublyLinkedDag}
import coop.rchain.models.BlockHash.BlockHash

/**
  * CasperBuffer manages block dependency graph.
  * Block is in scope of Casper Buffer until it is validated and added to DAG.
  */
trait CasperBufferStorage[F[_]] {
  def getParents(blockHash: BlockHash): F[Option[Set[BlockHash]]]
  def getChildren(blockHash: BlockHash): F[Option[Set[BlockHash]]]
  def getPendants: F[Set[BlockHash]]
  def addRelation(parent: BlockHash, child: BlockHash): F[Unit]
  def putPendant(block: BlockHash): F[Unit]
  def remove(blockHash: BlockHash): F[Unit]
  def contains(blockHash: BlockHash): F[Boolean]
  def isPendant(blockHash: BlockHash): F[Boolean]
  def size: F[Long]
  // TODO this is used only in single place, refactor and remove this method
  def toDoublyLinkedDag: F[DoublyLinkedDag[BlockHash]]
}

object CasperBufferStorage {
  def apply[F[_]](implicit instance: CasperBufferStorage[F]): CasperBufferStorage[F] = instance
}
