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
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.protocol.{BlockMessage, Justification}
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, MetricsSemaphore}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.EquivocationRecord.SequenceNumber
import coop.rchain.models.Validator.Validator
import coop.rchain.models.{BlockHash, BlockMetadata, EquivocationRecord, Validator}
import coop.rchain.shared.syntax._
import coop.rchain.blockstorage.syntax._
import coop.rchain.dag.DagOps
import coop.rchain.shared.{Log, LogSource}
import coop.rchain.store.{KeyValueStoreManager, KeyValueTypedStore}

import scala.collection.immutable.SortedMap

final class BlockDagKeyValueStorage[F[_]: Concurrent: Log] private (
    lock: Semaphore[F],
    latestMessagesIndex: KeyValueTypedStore[F, Validator, BlockHash],
    lastFinalizedBlocks: KeyValueTypedStore[F, Validator, BlockHash],
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
      invalidBlocksSet: Set[BlockMetadata],
      finalizedBlocksSet: Set[BlockHash]
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

    def isFinalized(blockHash: BlockHash): F[Boolean] =
      finalizedBlocksSet.contains(blockHash).pure[F]

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

    override def parents(vertex: BlockHash): F[Option[Set[BlockHash]]] =
      lookup(vertex).map(_.map(_.parents.toSet))

    override def view(
        validator: Validator,
        justificationHashes: Set[BlockHash]
    ): F[BlockDagRepresentation[F]] = {
      // This only works when tips that are going to be used are subset of full latest messages
      // It means this API won't let to reconstruct some historical view, only truncate the most recent view,
      // having some latest messages excluded
      assert(justificationHashes.subsetOf(latestMessagesMap.values.toSet))

      val excessTips = latestMessagesMap.values.filterNot(justificationHashes).toList
      val activeValidators = latestMessagesMap.iterator
        .filter { case (_, hash) => justificationHashes.contains(hash) }
        .map { case (validator, _) => validator }
        .toSet

      for {
        excessMetas <- this.lookupUnsafe(excessTips).compile.toList
        toRemove <- DagOps
                     .bfTraverseF(excessMetas)(
                       m =>
                         this
                           .lookupUnsafe(m.parents)
                           .filterNot(pm => activeValidators.contains(pm.sender))
                           .compile
                           .toList
                     )
                     .toSet
        toRemoveByHeight = toRemove.map(m => (m.blockNum, m.blockHash)).groupBy(_._1).map {
          case (k, v) => (k, v.map(_._2))
        }
        parentsToAdjust = toRemove.flatMap(_.parents)
        allToRemove     = toRemove.map(_.blockHash)
      } yield KeyValueDagRepresentation(
        dagSet = dagSet diff allToRemove,
        latestMessagesMap = latestMessagesMap.filterNot {
          case (_, msg) => allToRemove.contains(msg)
        },
        childMap = parentsToAdjust.foldLeft(childMap)(
          (acc, p) => acc.updated(p, acc(p) -- allToRemove)
        ), //TODO this is quite inefficient
        heightMap = toRemoveByHeight.foldLeft(heightMap)((acc, lineToRemove) => {
          val height       = lineToRemove._1
          val msgsToRemove = lineToRemove._2
          acc.updated(height, acc(height).diff(msgsToRemove))
        }),
        invalidBlocksSet = invalidBlocksSet.filterNot(ib => allToRemove.contains(ib.blockHash)),
        finalizedBlocksSet = finalizedBlocksSet diff allToRemove
      )
    }
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

  /**
    * Calculate view on the DAG
    * @param latestMessagesToUse if none - returns view as per the most recent state of BlockDagStorage
    *                            if block supplied - returns view including only latestMessages
    * @return
    */
  private def representation: F[BlockDagRepresentation[F]] =
    for {
      // Take current DAG state / view of the DAG
      latestMessages     <- latestMessagesIndex.toMap
      dagSet             <- blockMetadataIndex.dagSet
      childMap           <- blockMetadataIndex.childMapData
      heightMap          <- blockMetadataIndex.heightMap
      invalidBlocks      <- invalidBlocksIndex.toMap.map(_.toSeq.map(_._2).toSet)
      finalizedBlocksSet <- blockMetadataIndex.finalizedBlockSet
    } yield KeyValueDagRepresentation(
      dagSet,
      latestMessages,
      childMap,
      heightMap,
      invalidBlocks,
      finalizedBlocksSet
    )

  def getRepresentation: F[BlockDagRepresentation[F]] =
    lock.withPermit(representation)

  def insert(
      block: BlockMessage,
      invalid: Boolean
  ): F[BlockDagRepresentation[F]] = {
    import cats.instances.list._
    import cats.instances.option._
    import coop.rchain.catscontrib.Catscontrib.ToBooleanF

    // Empty sender is valid for genesis
    val senderIsEmpty          = block.sender == ByteString.EMPTY
    val senderHasInvalidFormat = !senderIsEmpty && (block.sender.size() != Validator.Length)
    val sendersNewLM           = (block.sender, block.blockHash)

    val logAlreadyStored =
      Log[F].warn(s"Block ${PrettyPrinter.buildString(block, short = true)} is already stored.")

    val logEmptySender =
      Log[F].warn(s"Block ${PrettyPrinter.buildString(block, short = true)} sender is empty.")

    // Add LM either if there is no existing message for the sender, or if sequence number advances
    // - assumes block sender is not valid hash
    def shouldAddAsLatest: F[Boolean] =
      latestMessagesIndex
      // Try get sender's latest message
        .get(block.sender)
        // Get metadata from index
        .flatMap(_.traverse(blockMetadataIndex.getUnsafe))
        // Check if seq number is greater that existing
        .map(_.map(_.seqNum))
        // Evaluate option and result. Add if:
        // - latest message is not found, or
        // - is found with seq num greater then existing
        .map(lmSeqNumOpt => lmSeqNumOpt.isEmpty || lmSeqNumOpt.exists(block.seqNum >= _))

    def newLatestMessages: F[Map[Validator, BlockHash]] = {
      val newlyBondedSet = bonds(block)
        .map(_.validator)
        .toSet
        .diff(block.justifications.map(_.validator).toSet)
      for {
        // This filter is required to enable adding blocks backward from higher height to lower
        newlyBondedUnseen <- newlyBondedSet.toList.filterA(latestMessagesIndex.contains(_).not)
      } yield newlyBondedUnseen.map((_, block.blockHash)).toMap
    }

    def doInsert: F[Unit] = {
      val blockMetadata      = BlockMetadata.fromBlock(block, invalid)
      val blockHashIsInvalid = !(block.blockHash.size == BlockHash.Length)

      for {
        // Basic validation of input hash values
        _ <- BlockSenderIsMalformed(block).raiseError[F, Unit].whenA(senderHasInvalidFormat)
        // TODO: should we have special error type for block hash error also?
        //  Should this be checked before calling insert? Is DAG storage responsible for that?
        _ <- new Exception(
              s"Block hash (${PrettyPrinter.buildString(block.blockHash)}) is not correct length."
            ).raiseError[F, Unit]
              .whenA(blockHashIsInvalid)

        _ <- logEmptySender.whenA(senderIsEmpty)

        // Add block metadata
        _ <- blockMetadataIndex.add(blockMetadata)

        // Add deploys to deploy index storage
        deployHashes = deployData(block).map(_.sig).toList
        _            <- deployIndex.put(deployHashes.map(_ -> block.blockHash))

        // Update invalid index
        _ <- invalidBlocksIndex.put(blockMetadata.blockHash, blockMetadata).whenA(invalid)

        // Resolve if block should be added as the latest message for the block sender
        emptyLM = Map.empty[Validator, BlockHash].pure[F]
        newLatestFromSender <- if (!senderIsEmpty)
                                shouldAddAsLatest.ifM(Map(sendersNewLM).pure[F], emptyLM)
                              else emptyLM

        // Add/update validators latest messages
        newLatestFromNewValidators <- newLatestMessages

        // All new latest messages to add
        newLatestToAdd = newLatestFromNewValidators ++ newLatestFromSender

        // Add latest messages to DB
        _ <- latestMessagesIndex.put(newLatestToAdd.toList)
      } yield ()
    }

    lock.withPermit(
      blockMetadataIndex
        .contains(block.blockHash)
        .ifM(logAlreadyStored, doInsert) >> representation
    )
  }

  override def accessEquivocationsTracker[A](f: EquivocationsTracker[F] => F[A]): F[A] =
    lock.withPermit(f(KeyValueStoreEquivocationsTracker))

  def checkpoint(): F[Unit] = ().pure[F]

  def close(): F[Unit] = ().pure[F]

  def addFinalizedBlockHash(blockHash: BlockHash): F[Unit] =
    blockMetadataIndex.addFinalizedBlock(blockHash)

  override def setFinalizedHash(validator: Validator, blockHash: BlockHash): F[Unit] = ???

  override def getFinalizedHash(validator: Validator): F[BlockHash] = ???
}

object BlockDagKeyValueStorage {
  implicit private val BlockDagKeyValueStorage_FromFileMetricsSource: Source =
    Metrics.Source(BlockStorageMetricsSource, "dag-key-value-store")

  type LastFinalizedStorage[F[_]] = KeyValueTypedStore[F, Validator, BlockHash]
  val NotValidator = ByteString.EMPTY

  private final case class DagStores[F[_]](
      metadata: BlockMetadataStore[F],
      metadataDb: KeyValueTypedStore[F, BlockHash, BlockMetadata],
      equivocations: EquivocationTrackerStore[F],
      equivocationsDb: KeyValueTypedStore[F, (Validator, SequenceNumber), Set[BlockHash]],
      latestMessages: KeyValueTypedStore[F, Validator, BlockHash],
      invalidBlocks: KeyValueTypedStore[F, BlockHash, BlockMetadata],
      deploys: KeyValueTypedStore[F, DeployId, BlockHash],
      lastFinalizedBlocks: LastFinalizedStorage[F]
  )

  private def createStores[F[_]: Concurrent: Log: Metrics](kvm: KeyValueStoreManager[F]) = {
    implicit val kvm_ = kvm
    for {
      // Block metadata map
      blockMetadataDb <- KeyValueStoreManager[F].database[BlockHash, BlockMetadata](
                          "block-metadata",
                          codecBlockHash,
                          codecBlockMetadata
                        )
      // last finalized block
      lastFinalizedStore <- KeyValueStoreManager[F].database[ByteString, BlockHash](
                             "last-finalized-block",
                             codecByteString,
                             codecBlockHash
                           )
      lastFinalizedBlock <- lastFinalizedStore.get(NotValidator)
      blockMetadataStore <- BlockMetadataStore[F](blockMetadataDb, lastFinalizedBlock)
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
      deployIndexDb,
      lastFinalizedStore
    )
  }

  def create[F[_]: Concurrent: Log: Metrics](kvm: KeyValueStoreManager[F]): F[BlockDagStorage[F]] =
    for {
      lock   <- MetricsSemaphore.single[F]
      stores <- createStores(kvm)
    } yield new BlockDagKeyValueStorage[F](
      lock,
      stores.latestMessages,
      stores.lastFinalizedBlocks,
      stores.metadata,
      stores.deploys,
      stores.invalidBlocks,
      stores.equivocations
    )
}
