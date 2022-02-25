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
import coop.rchain.blockstorage.syntax._
import coop.rchain.blockstorage.util.BlockMessageUtil._
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, MetricsSemaphore}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.EquivocationRecord.SequenceNumber
import coop.rchain.models.Validator.Validator
import coop.rchain.models.{BlockHash, BlockMetadata, EquivocationRecord, Validator}
import coop.rchain.shared.syntax._
import coop.rchain.models.syntax._
import coop.rchain.shared.{Base16, Log, LogSource}
import coop.rchain.store.{KeyValueStoreManager, KeyValueTypedStore}
import fs2.Stream

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
      invalidBlocksSet: Set[BlockMetadata],
      lastFinalizedBlockHash: BlockHash,
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

    /**
      * Truncate full DAG according to some view, defined by latest messages.
      */
    override def truncate(
        targetLatestMessages: Map[Validator, BlockHash],
        findLfb: Map[Validator, BlockHash] => F[BlockHash]
    ): F[BlockDagRepresentation[F]] = {

      val lmAll       = this.latestMessagesMap.values.toSet
      val seenSenders = targetLatestMessages.keySet

      for {
        lfb         <- findLfb(targetLatestMessages)
        lmFinalized <- this.latestFinalized(lfb, seenSenders).map(_.valuesIterator.toSet)
        lmSeen      <- targetLatestMessages.values.toList.traverse(this.lookupUnsafe).map(_.toSet)

        // for all known latest messages, collect all self justifications until first finalized message found
        toAdjust = {
          val seenSeqNums = lmSeen.map(m => (m.sender, m.seqNum)).toMap
          val finSeqNums  = lmFinalized.map(m => (m.sender, m.seqNum)).toMap
          val seen        = (m: BlockMetadata) => seenSeqNums.get(m.sender).exists(_ >= m.seqNum)
          val finalized   = (m: BlockMetadata) => finSeqNums.get(m.sender).exists(_ >= m.seqNum)

          Stream
            .unfoldLoopEval(lmAll.toVector) { messages =>
              for {
                metas <- messages.traverse(this.lookupUnsafe).map(_.filterNot(finalized))
                out   = metas.map(m => (m, !seen(m)))

                parents = metas.flatMap(_.parents).distinct
                next    = parents.nonEmpty.guard[Option].as(parents)
              } yield (Stream.emits(out.toList), next)
            }
            .flatten
            .fold((Map.empty[Long, Set[BlockHash]], Map.empty[Long, Set[BlockHash]], childMap)) {
              case ((removeAcc, unfinalizeAcc, childMapAcc), (m, shouldRemove)) =>
                val height = m.blockNum
                if (shouldRemove)
                  (
                    removeAcc + (height -> (removeAcc.getOrElse(height, Set()) + m.blockHash)),
                    unfinalizeAcc,
                    // Remove from children map key for message, adjust keys for parents.
                    // As some parent might be already removed by previous iterations, use filter.
                    m.parents.filter(childMapAcc.contains).foldLeft(childMapAcc - m.blockHash) {
                      case (a, p) => a + (p -> (a(p) - m.blockHash))
                    }
                  )
                else
                  (
                    removeAcc,
                    unfinalizeAcc + (height -> (unfinalizeAcc.getOrElse(height, Set()) + m.blockHash)),
                    childMapAcc
                  )
            }
            .compile
            .lastOrError
        }

        r                                        <- toAdjust
        (toRemove, toUnfinalize, newChildrenMap) = r
        excessSet                                = toRemove.flatMap { case (_, hashes) => hashes }.toSet
        nonFinalizedSet                          = toUnfinalize.flatMap { case (_, hashes) => hashes }.toSet

        // new truncated values
        truncatedDagSet     = this.dagSet diff excessSet
        truncatedInvalidSet = this.invalidBlocksSet.filter(m => dagSet.contains(m.blockHash))
        truncatedHeightMap = toRemove.foldLeft(this.heightMap) {
          case (acc, (height, hashes)) => acc.updated(height, acc(height) diff hashes)
        }
        truncatedFinalizesSet = finalizedBlocksSet diff excessSet diff nonFinalizedSet

        view = this.copy(
          lastFinalizedBlockHash = lfb,
          finalizedBlocksSet = truncatedFinalizesSet,
          dagSet = truncatedDagSet,
          childMap = newChildrenMap,
          latestMessagesMap = targetLatestMessages,
          heightMap = truncatedHeightMap,
          invalidBlocksSet = truncatedInvalidSet
        )
      } yield view
    }

    override def lastFinalizedBlock: BlockHash = lastFinalizedBlockHash

    // latestBlockNumber, topoSort and lookupByDeployId are only used in BlockAPI.
    // Do they need to be part of the DAG current state or they can be moved to DAG storage directly?

    private def getMaxHeight = if (heightMap.nonEmpty) heightMap.last._1 + 1L else 0L

    def latestBlockNumber: F[Long] =
      getMaxHeight.pure[F]

    def isFinalized(blockHash: BlockHash): F[Boolean] =
      finalizedBlocksSet.contains(blockHash).pure[F]

    override def find(truncatedHash: String): F[Option[BlockHash]] = Sync[F].delay {
      if (truncatedHash.length % 2 == 0) {
        val truncatedByteString = truncatedHash.unsafeHexToByteString
        dagSet.find(hash => hash.startsWith(truncatedByteString))
      } else {
        // if truncatedHash is odd length string we cannot convert it to ByteString with 8 bit resolution
        // because each symbol has 4 bit resolution. Need to make a string of even length by removing the last symbol,
        // then find all the matching hashes and choose one that matches the full truncatedHash string
        val truncatedByteString = truncatedHash.dropRight(1).unsafeHexToByteString
        dagSet
          .filter(_.startsWith(truncatedByteString))
          .find(_.toHexString.startsWith(truncatedHash))
      }
    }

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
      deployIndex.get1(deployId)

    override def nonFinalizedSet: Set[BlockHash] =
      dagSet diff finalizedBlocksSet
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
      latestMessages     <- latestMessagesIndex.toMap
      dagSet             <- blockMetadataIndex.dagSet
      childMap           <- blockMetadataIndex.childMapData
      heightMap          <- blockMetadataIndex.heightMap
      invalidBlocks      <- invalidBlocksIndex.toMap.map(_.toSeq.map(_._2).toSet)
      lastFinalizedBlock <- blockMetadataIndex.lastFinalizedBlock
      finalizedBlocksSet <- blockMetadataIndex.finalizedBlockSet
    } yield KeyValueDagRepresentation(
      dagSet,
      latestMessages,
      childMap,
      heightMap,
      invalidBlocks,
      lastFinalizedBlock,
      finalizedBlocksSet
    )

  def getRepresentation: F[BlockDagRepresentation[F]] =
    lock.withPermit(representation)

  def insert(
      block: BlockMessage,
      invalid: Boolean,
      approved: Boolean
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
        .get1(block.sender)
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

        // if block added as approved, record it as directly finalized.
        _ <- blockMetadataIndex.recordFinalized(blockMetadata.blockHash, Set.empty).whenA(approved)

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

  /** Record that some hash is directly finalized (detected by finalizer and becomes LFB). */
  def recordDirectlyFinalized(
      directlyFinalizedHash: BlockHash,
      finalizationEffect: Set[BlockHash] => F[Unit]
  ): F[Unit] =
    // Lock here is a safeguard for persisting changes in BlockMetadataIndex which can happen concurrently when
    // blocks are replayed in parallel
    lock.withPermit(
      for {
        dag    <- representation
        errMsg = s"Attempting to finalize nonexistent hash ${PrettyPrinter.buildString(directlyFinalizedHash)}."
        _ <- dag
              .contains(directlyFinalizedHash)
              .ifM(().pure, new Exception(errMsg).raiseError)
        // all non finalized ancestors should be finalized as well (indirectly)
        indirectlyFinalized <- dag.ancestors(List(directlyFinalizedHash), dag.isFinalized(_).not)
        // invoke effects
        _ <- finalizationEffect(indirectlyFinalized + directlyFinalizedHash)
        // persist finalization
        _ <- blockMetadataIndex.recordFinalized(directlyFinalizedHash, indirectlyFinalized)
      } yield ()
    )
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

  private def createStores[F[_]: Concurrent: Log: Metrics](kvm: KeyValueStoreManager[F]) = {
    implicit val kvm_ = kvm
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
  }

  def create[F[_]: Concurrent: Log: Metrics](kvm: KeyValueStoreManager[F]): F[BlockDagStorage[F]] =
    for {
      lock   <- MetricsSemaphore.single[F]
      stores <- createStores(kvm)
    } yield new BlockDagKeyValueStorage[F](
      lock,
      stores.latestMessages,
      stores.metadata,
      stores.deploys,
      stores.invalidBlocks,
      stores.equivocations
    )
}
