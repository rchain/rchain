package coop.rchain.blockstorage

import cats.effect.concurrent.{Ref, Semaphore}
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import coop.rchain.blockstorage.BlockDagRepresentation.Validator
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.blockstorage.util.BlockMessageUtil.parentHashes
import coop.rchain.blockstorage.util.TopologicalSortUtil
import coop.rchain.casper.protocol.{BlockMessage, Bond}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.shared.Log

import scala.collection.immutable.HashSet

final class InMemBlockDagStorage[F[_]: Concurrent: Sync: Log: BlockStore](
    lock: Semaphore[F],
    latestMessagesRef: Ref[F, Map[Validator, BlockHash]],
    childMapRef: Ref[F, Map[BlockHash, Set[BlockHash]]],
    dataLookupRef: Ref[F, Map[BlockHash, BlockMetadata]],
    topoSortRef: Ref[F, Vector[Vector[BlockHash]]]
) extends BlockDagStorage[F] {
  final case class InMemBlockDagRepresentation(
      latestMessagesMap: Map[Validator, BlockHash],
      childMap: Map[BlockHash, Set[BlockHash]],
      dataLookup: Map[BlockHash, BlockMetadata],
      topoSortVector: Vector[Vector[BlockHash]]
  ) extends BlockDagRepresentation[F] {
    def children(blockHash: BlockHash): F[Option[Set[BlockHash]]] =
      childMap.get(blockHash).pure[F]
    def lookup(blockHash: BlockHash): F[Option[BlockMetadata]] =
      dataLookup.get(blockHash).pure[F]
    def contains(blockHash: BlockHash): F[Boolean] =
      dataLookup.contains(blockHash).pure[F]
    def topoSort(startBlockNumber: Long): F[Vector[Vector[BlockHash]]] =
      topoSortVector.drop(startBlockNumber.toInt).pure[F]
    def topoSortTail(tailLength: Int): F[Vector[Vector[BlockHash]]] =
      topoSortVector.takeRight(tailLength).pure[F]
    def deriveOrdering(startBlockNumber: Long): F[Ordering[BlockMetadata]] =
      topoSort(startBlockNumber).map { topologicalSorting =>
        val order = topologicalSorting.flatten.zipWithIndex.toMap
        Ordering.by(b => order(b.blockHash))
      }
    def latestMessageHash(validator: Validator): F[Option[BlockHash]] =
      latestMessagesMap.get(validator).pure[F]
    def latestMessage(validator: Validator): F[Option[BlockMetadata]] =
      latestMessagesMap.get(validator).flatTraverse(lookup)
    def latestMessageHashes: F[Map[Validator, BlockHash]] =
      latestMessagesMap.pure[F]
    def latestMessages: F[Map[Validator, BlockMetadata]] =
      latestMessagesMap.toList
        .traverse {
          case (validator, hash) => lookup(hash).map(validator -> _.get)
        }
        .map(_.toMap)
  }

  override def getRepresentation: F[BlockDagRepresentation[F]] =
    for {
      _              <- lock.acquire
      latestMessages <- latestMessagesRef.get
      childMap       <- childMapRef.get
      dataLookup     <- dataLookupRef.get
      topoSort       <- topoSortRef.get
      _              <- lock.release
    } yield InMemBlockDagRepresentation(latestMessages, childMap, dataLookup, topoSort)
  override def insert(block: BlockMessage): F[Unit] =
    genericInsert(block, normalLatestMessagesStrategy)
  private def normalLatestMessagesStrategy(
      block: BlockMessage,
      latestMessages: Map[Validator, BlockHash]
  ): Map[Validator, BlockHash] =
    latestMessages.updated(block.sender, block.blockHash)
  override def insertGenesis(genesis: BlockMessage): F[Unit] =
    genericInsert(genesis, genesisLatestMessagesStrategy)
  private def genesisLatestMessagesStrategy(
      genesis: BlockMessage,
      latestMessages: Map[Validator, BlockHash]
  ): Map[Validator, BlockHash] = {
    val genesisBonds = (for {
      bd <- genesis.body
      ps <- bd.state
    } yield ps.bonds).getOrElse(List.empty[Bond])
    val initialLatestMessages = genesisBonds.map(_.validator -> genesis).toMap
    initialLatestMessages.toList.foldLeft(latestMessages) {
      case (acc, (validator, block)) => acc.updated(validator, block.blockHash)
    }
  }
  private def genericInsert(
      block: BlockMessage,
      latestMessagesStrategy: (BlockMessage, Map[Validator, BlockHash]) => Map[Validator, BlockHash]
  ): F[Unit] =
    for {
      _ <- lock.acquire
      _ <- dataLookupRef.update(_.updated(block.blockHash, BlockMetadata.fromBlock(block)))
      _ <- childMapRef.update(
            childMap =>
              parentHashes(block).foldLeft(childMap) {
                case (acc, p) =>
                  val currChildren = acc.getOrElse(p, HashSet.empty[BlockHash])
                  acc.updated(p, currChildren + block.blockHash)
              }
          )
      _ <- topoSortRef.update(topoSort => TopologicalSortUtil.update(topoSort, 0L, block))
      _ <- latestMessagesRef.update(latestMessages => latestMessagesStrategy(block, latestMessages))
      _ <- lock.release
    } yield ()
  override def checkpoint(): F[Unit] = ().pure[F]
  override def clear(): F[Unit] =
    for {
      _ <- lock.acquire
      _ <- dataLookupRef.set(Map.empty)
      _ <- childMapRef.set(Map.empty)
      _ <- topoSortRef.set(Vector.empty)
      _ <- latestMessagesRef.set(Map.empty)
      _ <- lock.release
    } yield ()
  override def close(): F[Unit] = ().pure[F]
}

object InMemBlockDagStorage {
  def create[F[_]: Concurrent: Sync: Log: BlockStore]: F[InMemBlockDagStorage[F]] =
    for {
      lock              <- Semaphore[F](1)
      latestMessagesRef <- Ref.of[F, Map[Validator, BlockHash]](Map.empty)
      childMapRef       <- Ref.of[F, Map[BlockHash, Set[BlockHash]]](Map.empty)
      dataLookupRef     <- Ref.of[F, Map[BlockHash, BlockMetadata]](Map.empty)
      topoSortRef       <- Ref.of[F, Vector[Vector[BlockHash]]](Vector.empty)
    } yield
      new InMemBlockDagStorage[F](
        lock,
        latestMessagesRef,
        childMapRef,
        dataLookupRef,
        topoSortRef
      )
}
