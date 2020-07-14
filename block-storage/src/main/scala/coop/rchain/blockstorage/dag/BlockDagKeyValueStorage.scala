package coop.rchain.blockstorage.dag

import cats.effect.concurrent.Semaphore
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage._
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.dag.BlockMetadataStore.BlockMetadataStore
import coop.rchain.blockstorage.dag.EquivocationTrackerStore.EquivocationTrackerStore
import coop.rchain.blockstorage.dag.codecs._
import coop.rchain.blockstorage.util.BlockMessageUtil._
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.crypto.codec.Base16
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, MetricsSemaphore}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.EquivocationRecord.SequenceNumber
import coop.rchain.models.Validator.Validator
import coop.rchain.models.{BlockHash, BlockMetadata, EquivocationRecord, Validator}
import coop.rchain.shared.syntax._
import coop.rchain.shared.{Log, LogSource}
import coop.rchain.store.{KeyValueStoreManager, KeyValueTypedStore}

import scala.collection.immutable.SortedMap

final class BlockDagKeyValueStorage[F[_]: Concurrent: Log] private (
    lock: Semaphore[F],
    latestMessagesIndex: KeyValueTypedStore[F, Validator, BlockHash],
    blockMetadataIndex: BlockMetadataStore[F],
    deployIndex: KeyValueTypedStore[F, DeployId, BlockHash],
    invalidBlocksIndex: KeyValueTypedStore[F, BlockHash, BlockMetadata],
    equivocationTrackerIndex: EquivocationTrackerStore[F]
) extends BlockDagStorage[F] {
  implicit private val logSource: LogSource = LogSource(BlockDagKeyValueStorage.getClass)

  private case class KeyValueDagRepresentation(
      dagSet: Set[BlockHash],
      latestMessagesMap: Map[Validator, BlockHash],
      childMap: Map[BlockHash, Set[BlockHash]],
      heightMap: SortedMap[Long, Set[BlockHash]],
      invalidBlocksSet: Set[BlockMetadata]
  ) extends BlockDagRepresentation[F] {

    def lookup(blockHash: BlockHash): F[Option[BlockMetadata]] =
      if (dagSet.contains(blockHash)) blockMetadataIndex.get(blockHash)
      else none[BlockMetadata].pure[F]

    def contains(blockHash: BlockHash): F[Boolean] =
      (blockHash.size == BlockHash.Length && dagSet.contains(blockHash)).pure[F]

    def children(blockHash: BlockHash): F[Option[Set[BlockHash]]] =
      childMap.get(blockHash).pure[F]

    def latestMessageHash(validator: Validator): F[Option[BlockHash]] =
      latestMessagesMap.get(validator).pure[F]

    def latestMessageHashes: F[Map[Validator, BlockHash]] = latestMessagesMap.pure[F]

    def invalidBlocks: F[Set[BlockMetadata]] = invalidBlocksSet.pure[F]

    // latestBlockNumber, topoSort and lookupByDeployId are only used in BlockAPI.
    // Do they need to be part of the DAG current state or they can be moved to DAG storage directly?

    private def getMaxHeight = if (heightMap.nonEmpty) heightMap.last._1 + 1L else 0L

    def latestBlockNumber: F[Long] =
      getMaxHeight.pure[F]

    def topoSort(
        startBlockNumber: Long,
        maybeEndBlockNumber: Option[Long]
    ): F[Vector[Vector[BlockHash]]] = {
      val maxNumber   = getMaxHeight
      val startNumber = Math.max(0, startBlockNumber)
      val endNumber   = maybeEndBlockNumber.map(Math.min(maxNumber, _)).getOrElse(maxNumber)
      if (startNumber >= 0 && startNumber <= endNumber) {
        Sync[F].delay(
          heightMap
            .filterKeys(h => h >= startNumber && h <= endNumber)
            .map { case (_, v) => v.toVector }
            .toVector
        )
      } else {
        Sync[F].raiseError(
          TopoSortFragmentParameterError(startNumber, endNumber)
        )
      }
    }

    def lookupByDeployId(deployId: DeployId): F[Option[BlockHash]] =
      deployIndex.get(deployId)
  }

  private object KeyValueStoreEquivocationsTracker extends EquivocationsTracker[F] {
    override def equivocationRecords: F[Set[EquivocationRecord]] =
      equivocationTrackerIndex.data

    override def insertEquivocationRecord(record: EquivocationRecord): F[Unit] =
      equivocationTrackerIndex.add(record)

    override def updateEquivocationRecord(
        record: EquivocationRecord,
        blockHash: BlockHash
    ): F[Unit] = {
      val updatedEquivocationDetectedBlockHashes =
        record.equivocationDetectedBlockHashes + blockHash
      val newRecord =
        record.copy(equivocationDetectedBlockHashes = updatedEquivocationDetectedBlockHashes)
      equivocationTrackerIndex.add(newRecord)
    }
  }

  private def representation: F[BlockDagRepresentation[F]] =
    for {
      // Take current DAG state / view of the DAG
      latestMessages <- latestMessagesIndex.toMap
      dagSet         <- blockMetadataIndex.dagSet
      childMap       <- blockMetadataIndex.childMapData
      heightMap      <- blockMetadataIndex.heightMap
      invalidBlocks  <- invalidBlocksIndex.toMap.map(_.toSeq.map(_._2).toSet)
    } yield KeyValueDagRepresentation(
      dagSet,
      latestMessages,
      childMap,
      heightMap,
      invalidBlocks
    )

  def getRepresentation: F[BlockDagRepresentation[F]] =
    lock.withPermit(representation)

  def insert(
      block: BlockMessage,
      genesis: BlockMessage,
      invalid: Boolean
  ): F[BlockDagRepresentation[F]] =
    lock.withPermit(
      for {
        alreadyStored <- blockMetadataIndex.contains(block.blockHash)
        _ <- if (alreadyStored) {
              Log[F].warn(s"Block ${Base16.encode(block.blockHash.toByteArray)} is already stored")
            } else {
              val blockMetadata = BlockMetadata.fromBlock(block, invalid)
              assert(block.blockHash.size == BlockHash.Length)
              for {
                _ <- if (invalid) invalidBlocksIndex.put(blockMetadata.blockHash, blockMetadata)
                    else ().pure[F]
                //Block which contains newly bonded validators will not
                //have those validators in its justification
                newValidators = bonds(block)
                  .map(_.validator)
                  .toSet
                  .diff(block.justifications.map(_.validator).toSet)
                newValidatorsLatestMessages = newValidators.map(v => (v, genesis.blockHash))
                newValidatorsWithSenderLatestMessages <- if (block.sender.isEmpty) {
                                                          // Ignore empty sender for special cases such as genesis block
                                                          Log[F].warn(
                                                            s"Block ${Base16.encode(block.blockHash.toByteArray)} sender is empty"
                                                          ) >> newValidatorsLatestMessages.pure[F]
                                                        } else if (block.sender
                                                                     .size() == Validator.Length) {
                                                          (newValidatorsLatestMessages + (
                                                            (
                                                              block.sender,
                                                              block.blockHash
                                                            )
                                                          )).pure[F]
                                                        } else {
                                                          Sync[F].raiseError[Set[
                                                            (ByteString, ByteString)
                                                          ]](
                                                            BlockSenderIsMalformed(block)
                                                          )
                                                        }
                deployHashes = deployData(block).map(_.sig).toList
                // Add deploys to deploy index storage
                _ <- deployIndex.put(deployHashes.map(_ -> block.blockHash))
                // Add/update validators latest messages
                _ <- latestMessagesIndex.put(newValidatorsWithSenderLatestMessages.toList)
                // Add block metadata
                _ <- blockMetadataIndex.add(blockMetadata)
              } yield ()
            }
        dag <- representation
      } yield dag
    )

  override def accessEquivocationsTracker[A](f: EquivocationsTracker[F] => F[A]): F[A] =
    lock.withPermit(f(KeyValueStoreEquivocationsTracker))

  def checkpoint(): F[Unit] = ().pure[F]

  def close(): F[Unit] = ().pure[F]
}

object BlockDagKeyValueStorage {
  implicit private val BlockDagKeyValueStorage_FromFileMetricsSource: Source =
    Metrics.Source(BlockStorageMetricsSource, "dag-key-value-store")

  private final case class DagStores[F[_]](
      metadata: BlockMetadataStore[F],
      metadataDb: KeyValueTypedStore[F, BlockHash, BlockMetadata],
      equivocations: EquivocationTrackerStore[F],
      equivocationsDb: KeyValueTypedStore[F, (Validator, SequenceNumber), Set[BlockHash]],
      latestMessages: KeyValueTypedStore[F, Validator, BlockHash],
      invalidBlocks: KeyValueTypedStore[F, BlockHash, BlockMetadata],
      deploys: KeyValueTypedStore[F, DeployId, BlockHash]
  )

  private def createStores[F[_]: Concurrent: KeyValueStoreManager: Log: Metrics] =
    for {
      // Block metadata map
      blockMetadataDb <- KeyValueStoreManager[F].database[BlockHash, BlockMetadata](
                          "block-metadata",
                          codecBlockHash,
                          codecBlockMetadata
                        )
      blockMetadataStore <- BlockMetadataStore[F](blockMetadataDb)
      // Equivocation tracker map
      equivocationTrackerDb <- KeyValueStoreManager[F]
                                .database[(Validator, SequenceNumber), Set[BlockHash]](
                                  "equivocation-tracker",
                                  codecValidator ~ codecSeqNum,
                                  codecBlockHashSet
                                )
      equivocationTrackerIndex <- EquivocationTrackerStore[F](equivocationTrackerDb)
      // Latest messages map
      latestMessagesDb <- KeyValueStoreManager[F].database[Validator, BlockHash](
                           "latest-messages",
                           codecValidator,
                           codecBlockHash
                         )
      // Invalid blocks map
      invalidBlocksDb <- KeyValueStoreManager[F].database[BlockHash, BlockMetadata](
                          "invalid-blocks",
                          codecBlockHash,
                          codecBlockMetadata
                        )
      // Deploy map
      deployIndexDb <- KeyValueStoreManager[F].database[DeployId, BlockHash](
                        "deploy-index",
                        codecDeployId,
                        codecBlockHash
                      )
    } yield DagStores(
      blockMetadataStore,
      blockMetadataDb,
      equivocationTrackerIndex,
      equivocationTrackerDb,
      latestMessagesDb,
      invalidBlocksDb,
      deployIndexDb
    )

  def create[F[_]: Concurrent: KeyValueStoreManager: Log: Metrics]: F[BlockDagStorage[F]] =
    for {
      lock   <- MetricsSemaphore.single[F]
      stores <- createStores
    } yield new BlockDagKeyValueStorage[F](
      lock,
      stores.latestMessages,
      stores.metadata,
      stores.deploys,
      stores.invalidBlocks,
      stores.equivocations
    )

  def importFromFileStorage[F[_]: Concurrent: KeyValueStoreManager: Log: Metrics](
      config: BlockDagFileStorage.Config
  ): F[Unit] =
    for {
      _ <- Log[F].warn(s"Starting DAG file storage migration, loading existing data.")

      // Load old DAG file storage
      oldStores <- BlockDagFileStorage.createStores(config)
      // Old indexes
      (_, oldLatestMessages, oldMetadata, oldDeploys, oldInvalidBlocks, oldEquivocations) = oldStores

      _ <- Log[F].warn(s"Create new DAG storage.")

      // Create new stores
      stores <- createStores

      _ <- Log[F].warn(s"Migrate metadata index.")

      // Migrate metadata index
      oldMetadataMap <- oldMetadata.blockMetadataData
      _              <- stores.metadataDb.put(oldMetadataMap.toSeq)

      _ <- Log[F].warn(s"Migrate latest messages index.")

      // Migrate latest messages index
      oldLatestMessages <- oldLatestMessages.data
      _                 <- stores.latestMessages.put(oldLatestMessages.toSeq)

      _ <- Log[F].warn(s"Migrate invalid blocks index.")

      // Migrate invalid blocks index
      oldInvalidBlocksRaw  <- oldInvalidBlocks.data
      oldInvalidBlocksData = oldInvalidBlocksRaw.keys.map(b => (b.blockHash, b)).toSeq
      _                    <- stores.invalidBlocks.put(oldInvalidBlocksData)

      _ <- Log[F].warn(s"Migrate equivocations tracker index.")

      // Migrate equivocation tracker index
      oldEquivocationsRaw <- oldEquivocations.data
      oldEquivocationsData = oldEquivocationsRaw
        .map(
          x => ((x.equivocator, x.equivocationBaseBlockSeqNum), x.equivocationDetectedBlockHashes)
        )
        .toSeq
      _ <- stores.equivocationsDb.put(oldEquivocationsData)

      _ <- Log[F].warn(s"Migrate deploys index.")

      // Migrate deploys index
      oldDeploysData <- oldDeploys.data
      _              <- stores.deploys.put(oldDeploysData.toSeq)

      _ <- Log[F].warn(s"DAG storage migration successful.")
    } yield ()

}
