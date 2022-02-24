package coop.rchain.blockstorage.casperbuffer

import cats.effect.Concurrent
import cats.effect.concurrent.{Ref, Semaphore}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.blockstorage.dag.codecs._
import coop.rchain.shared.syntax._
import coop.rchain.blockstorage.util.{
  BlockDependencyDag,
  DoublyLinkedDag,
  DoublyLinkedDagOperations
}
import coop.rchain.blockstorage.BlockStorageMetricsSource
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, MetricsSemaphore}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.Log
import coop.rchain.store.{KeyValueStoreManager, KeyValueTypedStore}
import scodec.Codec

/**
  * @param parentsStore - persistent map {hash -> parents set}
  * @param blockDependencyDag - in-memory dependency DAG, recreated from parentsStore on node startup
  */
final class CasperBufferKeyValueStorage[F[_]: Concurrent: Log] private (
    parentsStore: KeyValueTypedStore[F, BlockHash, Set[BlockHash]],
    blockDependencyDag: Ref[F, DoublyLinkedDag[BlockHash]],
    lock: Semaphore[F]
) extends CasperBufferStorage[F] {

  override def addRelation(parent: BlockHash, child: BlockHash): F[Unit] =
    lock.withPermit(
      for {
        parents <- parentsStore.get1(child).map(_.getOrElse(Set.empty[BlockHash]))
        _       <- parentsStore.put(child, parents + parent)
        _ <- blockDependencyDag.update(
              curState => DoublyLinkedDagOperations.add(curState, parent, child)
            )
      } yield ()
    )

  def putPendant(block: BlockHash): F[Unit] = {
    val tempBlock = ByteString.copyFromUtf8("tempblock")
    for {
      _ <- addRelation(tempBlock, block)
      _ <- remove(tempBlock)
    } yield ()
  }

  override def remove(hash: BlockHash): F[Unit] =
    lock.withPermit(
      for {
        curState                                  <- blockDependencyDag.get
        (newState, hashesAffected, hashesRemoved) = DoublyLinkedDagOperations.remove(curState, hash)
        _                                         <- blockDependencyDag.set(newState)
        changes                                   = hashesAffected.map(h => (h, newState.childToParentAdjacencyList(h))).toSeq
        deletes                                   = hashesRemoved.toSeq
        _                                         <- parentsStore.put(changes)
        // If node crush here - KV store end up in inconsistent state. What's the good way to handle it?
        _ <- parentsStore.delete(deletes)
      } yield ()
    )

  override def getParents(blockHash: BlockHash): F[Option[Set[BlockHash]]] =
    blockDependencyDag.get.map(_.childToParentAdjacencyList.get(blockHash))

  override def getChildren(blockHash: BlockHash): F[Option[Set[BlockHash]]] =
    blockDependencyDag.get.map(_.parentToChildAdjacencyList.get(blockHash))

  override def getPendants: F[Set[BlockHash]] =
    blockDependencyDag.get.map(_.dependencyFree)

  // Block is considered to be in CasperBuffer when there is a records about its parents
  override def contains(blockHash: BlockHash): F[Boolean] =
    blockDependencyDag.get.map(
      bdd => bdd.childToParentAdjacencyList.get(blockHash).exists(_.nonEmpty)
    )

  override def toDoublyLinkedDag: F[DoublyLinkedDag[BlockHash]] = blockDependencyDag.get

  override def size: F[Long] =
    blockDependencyDag.get.map(_.childToParentAdjacencyList.size.toLong)

  override def isPendant(blockHash: BlockHash): F[Boolean] =
    getPendants.map(_.contains(blockHash))
}

object CasperBufferKeyValueStorage {
  implicit private val ms: Source =
    Metrics.Source(BlockStorageMetricsSource, "dag-key-value-store")

  def create[F[_]: Concurrent: Log: Metrics](
      kvm: KeyValueStoreManager[F]
  ): F[CasperBufferStorage[F]] = {

    def recreateInMemStore(
        parentsStore: KeyValueTypedStore[F, BlockHash, Set[BlockHash]],
        initState: DoublyLinkedDag[BlockHash] = BlockDependencyDag.empty
    ): F[DoublyLinkedDag[BlockHash]] =
      for {
        parentsMap <- parentsStore.toMap
        inMemStore = parentsMap.keySet.toList.foldLeft(initState) { (bdd, key) =>
          val parents = parentsMap(key)
          parents.toList.foldLeft(bdd)(
            (bdd, p) => DoublyLinkedDagOperations.add(bdd, p, key)
          )
        }
      } yield inMemStore

    for {
      parentsStore <- kvm.database[BlockHash, Set[BlockHash]](
                       "parents-map",
                       codecBlockHash,
                       codecBlockHashSet
                     )
      inMemStore    <- recreateInMemStore(parentsStore)
      inMemStoreRef <- Ref.of[F, DoublyLinkedDag[BlockHash]](inMemStore)
      lock          <- MetricsSemaphore.single[F]
    } yield new CasperBufferKeyValueStorage[F](
      parentsStore,
      inMemStoreRef,
      lock
    )
  }

}
