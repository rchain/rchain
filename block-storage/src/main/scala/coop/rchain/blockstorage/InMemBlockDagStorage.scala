package coop.rchain.blockstorage

import cats.effect.concurrent.{Ref, Semaphore}
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockDagRepresentation.Validator
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.blockstorage.StorageError.StorageErr
import coop.rchain.blockstorage.util.BlockMessageUtil.{bonds, parentHashes}
import coop.rchain.blockstorage.util.{BlockMessageUtil, TopologicalSortUtil}
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.crypto.codec.Base16
import coop.rchain.models.{BlockMetadata, EquivocationRecord}
import coop.rchain.models.EquivocationRecord.SequenceNumber
import coop.rchain.shared.Log

import scala.collection.immutable.HashSet

final class InMemBlockDagStorage[F[_]: Concurrent: Sync: Log: BlockStore](
    lock: Semaphore[F],
    latestMessagesRef: Ref[F, Map[Validator, BlockHash]],
    childMapRef: Ref[F, Map[BlockHash, Set[BlockHash]]],
    dataLookupRef: Ref[F, Map[BlockHash, BlockMetadata]],
    topoSortRef: Ref[F, Vector[Vector[BlockHash]]],
    equivocationsTrackerRef: Ref[F, Set[EquivocationRecord]],
    invalidBlocksRef: Ref[F, Set[BlockMetadata]]
) extends BlockDagStorage[F] {
  private case class InMemBlockDagRepresentation(
      latestMessagesMap: Map[Validator, BlockHash],
      childMap: Map[BlockHash, Set[BlockHash]],
      dataLookup: Map[BlockHash, BlockMetadata],
      topoSortVector: Vector[Vector[BlockHash]],
      invalidBlocksSet: Set[BlockMetadata]
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
    def invalidBlocks: F[Set[BlockMetadata]] =
      invalidBlocksSet.pure[F]
  }

  object InMemEquivocationsTracker extends EquivocationsTracker[F] {
    override def equivocationRecords: F[Set[EquivocationRecord]] =
      equivocationsTrackerRef.get
    override def insertEquivocationRecord(record: EquivocationRecord): F[Unit] =
      equivocationsTrackerRef.update(_ + record)
    override def updateEquivocationRecord(
        record: EquivocationRecord,
        blockHash: BlockHash
    ): F[Unit] = {
      val updatedEquivocationDetectedBlockHashes =
        record.equivocationDetectedBlockHashes + blockHash
      equivocationsTrackerRef.update(
        _ - record +
          record.copy(equivocationDetectedBlockHashes = updatedEquivocationDetectedBlockHashes)
      )
    }
  }

  override def getRepresentation: F[BlockDagRepresentation[F]] =
    for {
      _              <- lock.acquire
      latestMessages <- latestMessagesRef.get
      childMap       <- childMapRef.get
      dataLookup     <- dataLookupRef.get
      topoSort       <- topoSortRef.get
      invalidBlocks  <- invalidBlocksRef.get
      _              <- lock.release
    } yield InMemBlockDagRepresentation(
      latestMessages,
      childMap,
      dataLookup,
      topoSort,
      invalidBlocks
    )

  override def insert(
      block: BlockMessage,
      genesis: BlockMessage,
      invalid: Boolean
  ): F[BlockDagRepresentation[F]] =
    for {
      _             <- lock.acquire
      blockMetadata = BlockMetadata.fromBlock(block, invalid)
      _             <- dataLookupRef.update(_.updated(block.blockHash, blockMetadata))
      _ <- childMapRef.update(
            childMap =>
              parentHashes(block).foldLeft(childMap) {
                case (acc, p) =>
                  val currChildren = acc.getOrElse(p, HashSet.empty[BlockHash])
                  acc.updated(p, currChildren + block.blockHash)
              }
          )
      _ <- topoSortRef.update(topoSort => TopologicalSortUtil.update(topoSort, 0L, block))
      newValidators = bonds(block)
        .map(_.validator)
        .toSet
        .diff(block.justifications.map(_.validator).toSet)
      newValidatorsLatestMessages = newValidators.map(v => (v, genesis.blockHash))
      newValidatorsWithSenderLatestMessages <- if (block.sender.isEmpty) {
                                                // Ignore empty sender for special cases such as genesis block
                                                Log[F].warn(
                                                  s"Block ${Base16.encode(block.blockHash.toByteArray)} sender is empty"
                                                ) *> newValidatorsLatestMessages.pure[F]
                                              } else if (block.sender.size() == 32) {
                                                (newValidatorsLatestMessages + (
                                                  (
                                                    block.sender,
                                                    block.blockHash
                                                  )
                                                )).pure[F]
                                              } else {
                                                Sync[F].raiseError[Set[(ByteString, ByteString)]](
                                                  BlockSenderIsMalformed(block)
                                                )
                                              }
      _ <- latestMessagesRef.update { latestMessages =>
            newValidatorsWithSenderLatestMessages.foldLeft(latestMessages) {
              case (acc, (validator, blockHash)) => acc.updated(validator, blockHash)
            }
          }
      _   <- if (invalid) invalidBlocksRef.update(_ + blockMetadata) else ().pure[F]
      _   <- lock.release
      dag <- getRepresentation
    } yield dag

  override def accessEquivocationsTracker[A](f: EquivocationsTracker[F] => F[A]): F[A] =
    lock.withPermit(
      f(InMemEquivocationsTracker)
    )
  override def checkpoint(): F[Unit] = ().pure[F]
  override def clear(): F[Unit] =
    for {
      _ <- lock.acquire
      _ <- dataLookupRef.set(Map.empty)
      _ <- childMapRef.set(Map.empty)
      _ <- topoSortRef.set(Vector.empty)
      _ <- latestMessagesRef.set(Map.empty)
      _ <- equivocationsTrackerRef.set(Set.empty)
      _ <- lock.release
    } yield ()
  override def close(): F[Unit] = ().pure[F]
}

object InMemBlockDagStorage {
  def create[F[_]: Concurrent: Sync: Log: BlockStore]: F[InMemBlockDagStorage[F]] =
    for {
      lock                    <- Semaphore[F](1)
      latestMessagesRef       <- Ref.of[F, Map[Validator, BlockHash]](Map.empty)
      childMapRef             <- Ref.of[F, Map[BlockHash, Set[BlockHash]]](Map.empty)
      dataLookupRef           <- Ref.of[F, Map[BlockHash, BlockMetadata]](Map.empty)
      topoSortRef             <- Ref.of[F, Vector[Vector[BlockHash]]](Vector.empty)
      equivocationsTrackerRef <- Ref.of[F, Set[EquivocationRecord]](Set.empty)
      invalidBlocksRef        <- Ref.of[F, Set[BlockMetadata]](Set.empty)
    } yield new InMemBlockDagStorage[F](
      lock,
      latestMessagesRef,
      childMapRef,
      dataLookupRef,
      topoSortRef,
      equivocationsTrackerRef,
      invalidBlocksRef
    )
}
