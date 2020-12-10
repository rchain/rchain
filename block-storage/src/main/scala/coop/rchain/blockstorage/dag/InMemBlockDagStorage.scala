package coop.rchain.blockstorage.dag

import cats.effect.concurrent.{Ref, Semaphore}
import cats.effect.{Concurrent, Sync}
import cats.implicits._

import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.util.BlockMessageUtil.{bonds, deployData, parentHashes}
import coop.rchain.blockstorage.util.TopologicalSortUtil
import coop.rchain.blockstorage.{BlockSenderIsMalformed, BlockStorageMetricsSource, BlockStore}
import coop.rchain.casper.protocol.{BlockMessage, BlockMessageProto}
import coop.rchain.crypto.codec.Base16
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, MetricsSemaphore}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.{BlockHash, BlockMetadata, EquivocationRecord}
import coop.rchain.shared.Log

import scala.collection.immutable.HashSet

final class InMemBlockDagStorage[F[_]: Concurrent: Sync: Log](
    lock: Semaphore[F],
    latestMessagesRef: Ref[F, Map[Validator, BlockHash]],
    childMapRef: Ref[F, Map[BlockHash, Set[BlockHash]]],
    dataLookupRef: Ref[F, Map[BlockHash, BlockMetadata]],
    topoSortRef: Ref[F, Vector[Vector[BlockHash]]],
    blockHashesByDeployRef: Ref[F, Map[DeployId, BlockHash]],
    equivocationsTrackerRef: Ref[F, Set[EquivocationRecord]],
    invalidBlocksRef: Ref[F, Set[BlockMetadata]],
    finalizedBlocksRef: Ref[F, Set[BlockHash]]
) extends BlockDagStorage[F] {
  private case class InMemBlockDagRepresentation(
      latestMessagesMap: Map[Validator, BlockHash],
      childMap: Map[BlockHash, Set[BlockHash]],
      dataLookup: Map[BlockHash, BlockMetadata],
      topoSortVector: Vector[Vector[BlockHash]],
      blockHashesByDeploy: Map[DeployId, BlockHash],
      invalidBlocksSet: Set[BlockMetadata]
  ) extends BlockDagRepresentation[F] {
    def children(blockHash: BlockHash): F[Option[Set[BlockHash]]] =
      childMap.get(blockHash).pure[F]
    def lookup(blockHash: BlockHash): F[Option[BlockMetadata]] =
      dataLookup.get(blockHash).pure[F]
    def contains(blockHash: BlockHash): F[Boolean] =
      dataLookup.contains(blockHash).pure[F]
    def lookupByDeployId(deployId: DeployId): F[Option[BlockHash]] =
      blockHashesByDeploy.get(deployId).pure[F]
    def topoSort(
        startBlockNumber: Long,
        maybeEndBlockNumber: Option[Long]
    ): F[Vector[Vector[BlockHash]]] = {
      val endBlockNumber: Long = maybeEndBlockNumber.getOrElse(topoSortVector.length.toLong)
      topoSortVector.slice(startBlockNumber.toInt, endBlockNumber.toInt + 1).pure[F]
    }

    def isFinalized(blockHash: BlockHash): F[Boolean] =
      finalizedBlocksRef.get.map(_.contains(blockHash))

    def latestBlockNumber: F[Long] = (topoSortVector.length - 1L).pure[F]

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

    override def parents(vertex: BlockHash): F[Option[Set[BlockHash]]] =
      lookup(vertex).map(_.map(_.parents.toSet))
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
    lock.withPermit(getRepresentationInternal)

  private def getRepresentationInternal: F[BlockDagRepresentation[F]] =
    for {
      latestMessages    <- latestMessagesRef.get
      childMap          <- childMapRef.get
      dataLookup        <- dataLookupRef.get
      topoSort          <- topoSortRef.get
      blockHashByDeploy <- blockHashesByDeployRef.get
      invalidBlocks     <- invalidBlocksRef.get
    } yield InMemBlockDagRepresentation(
      latestMessages,
      childMap,
      dataLookup,
      topoSort,
      blockHashByDeploy,
      invalidBlocks
    )

  override def insert(
      block: BlockMessage,
      invalid: Boolean
  ): F[BlockDagRepresentation[F]] =
    lock.withPermit(
      for {
        blockMetadata <- Sync[F].delay(BlockMetadata.fromBlock(block, invalid))
        _             <- dataLookupRef.update(_.updated(block.blockHash, blockMetadata))
        _ <- childMapRef.update(
              childMap =>
                parentHashes(block).foldLeft(childMap) {
                  case (acc, p) =>
                    val currChildren = acc.getOrElse(p, HashSet.empty[BlockHash])
                    acc.updated(p, currChildren + block.blockHash)
                }
            )
        _ <- topoSortRef.update(topoSort => TopologicalSortUtil.update(topoSort, 0L, blockMetadata))
        newValidators = bonds(block)
          .map(_.validator)
          .toSet
          .diff(block.justifications.map(_.validator).toSet)
        newValidatorsLatestMessages = newValidators.map(v => (v, block.blockHash))
        newValidatorsWithSenderLatestMessages <- if (block.sender.isEmpty) {
                                                  // Ignore empty sender for special cases such as genesis block
                                                  Log[F].warn(
                                                    s"Block ${Base16.encode(block.blockHash.toByteArray)} sender is empty"
                                                  ) >> newValidatorsLatestMessages.pure[F]
                                                } else if (block.sender
                                                             .size() == BlockHash.Length) {
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
        deployHashes = deployData(block).map(_.sig).toList
        _ <- latestMessagesRef.update { latestMessages =>
              newValidatorsWithSenderLatestMessages.foldLeft(latestMessages) {
                case (acc, (validator, blockHash)) => acc.updated(validator, blockHash)
              }
            }
        _   <- if (invalid) invalidBlocksRef.update(_ + blockMetadata) else ().pure[F]
        _   <- blockHashesByDeployRef.update(_ ++ deployHashes.map(_ -> block.blockHash).toMap)
        dag <- getRepresentationInternal
      } yield dag
    )

  override def accessEquivocationsTracker[A](f: EquivocationsTracker[F] => F[A]): F[A] =
    lock.withPermit(
      f(InMemEquivocationsTracker)
    )
  override def checkpoint(): F[Unit] = ().pure[F]

  override def close(): F[Unit] = ().pure[F]

  override def addFinalizedBlockHash(blockHash: BlockHash): F[Unit] =
    finalizedBlocksRef.update(_ + blockHash)
}

object InMemBlockDagStorage {
  implicit private val InMemBlockDagStorageMetricsSource: Source =
    Metrics.Source(BlockStorageMetricsSource, "in-mem")

  def create[F[_]: Concurrent: Sync: Log: Metrics]: F[InMemBlockDagStorage[F]] =
    for {
      lock                    <- MetricsSemaphore.single[F]
      latestMessagesRef       <- Ref.of[F, Map[Validator, BlockHash]](Map.empty)
      childMapRef             <- Ref.of[F, Map[BlockHash, Set[BlockHash]]](Map.empty)
      dataLookupRef           <- Ref.of[F, Map[BlockHash, BlockMetadata]](Map.empty)
      topoSortRef             <- Ref.of[F, Vector[Vector[BlockHash]]](Vector.empty)
      blockHashesByDeployRef  <- Ref.of[F, Map[DeployId, BlockHash]](Map.empty)
      equivocationsTrackerRef <- Ref.of[F, Set[EquivocationRecord]](Set.empty)
      invalidBlocksRef        <- Ref.of[F, Set[BlockMetadata]](Set.empty)
      finalizedBlocksRef      <- Ref.of[F, Set[BlockHash]](Set.empty)
    } yield new InMemBlockDagStorage[F](
      lock,
      latestMessagesRef,
      childMapRef,
      dataLookupRef,
      topoSortRef,
      blockHashesByDeployRef,
      equivocationsTrackerRef,
      invalidBlocksRef,
      finalizedBlocksRef
    )
}
