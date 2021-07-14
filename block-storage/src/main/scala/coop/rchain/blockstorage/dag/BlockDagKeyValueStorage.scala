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
import coop.rchain.shared.{Log, LogSource}
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
      childMap.get(blockHash).map(_ intersect dagSet).pure[F]

    def latestMessageHash(validator: Validator): F[Option[BlockHash]] =
      latestMessagesMap.get(validator).pure[F]

    def latestMessageHashes: F[Map[Validator, BlockHash]] = latestMessagesMap.pure[F]

    def invalidBlocks: F[Set[BlockMetadata]] = invalidBlocksSet.pure[F]

    /**
      * Truncate full DAG according to some view, defined by latest messages.
      *
      * Please see example below: `-` here denote message that are not seen by target view.
      * Target latest messages are A B C D E. G - genesis
      *
      * Computing of excess messages can be divided into 3 parts:
      * 1. high excess - messages that has height above highest target latest message. This is done simply by
      * splitting height map.
      * 2. lmRange excess, which are messages inside the range of target latest messages, but not seen yet by
      * the view (are higher then some target latest message from the same sender).
      * This requires reading metadata which is more costly then 1.
      * 3. unseen senders excess (low excess) - Please note that the first column is full of `-`, which means target
      * message sender is ignoring this validator, but this particular node adds these
      * messages for some reason. It does not seem like a impossible scenario, and makes truncating logic a
      * bit more complex.
      *
      *   - - -   -
      *     -       _____  ^             high excess              ^
      *     B   -          v                                      v
      *     *   D E
      *     * C                        lmRange excess
      * - - *   *
      * - A * * * * _____  ^                                      ^
      * -   *   *          v   unseenSendersExcess (low excess)   v
      * -   * * * *
      * - * *   *
      *     G
      *
      */
    override def truncate(
        targetLatestMessages: Map[Validator, BlockHash]
    ): F[BlockDagRepresentation[F]] = {
      final case class HeightView(
          height: Long,
          excess: Iterator[ByteString],
          inView: Iterator[ByteString]
      )
      for {
        lmMetas <- targetLatestMessages.values.toStream.traverse(this.lookupUnsafe)
        (highestHeight, lowestHeight, lmPerValidatorHeight) = lmMetas.foldLeft(
          (0L, Long.MaxValue, Map.empty[ByteString, Long])
        ) {
          case ((curHH, curLH, curLVH), meta) =>
            val newHH  = Math.max(curHH, meta.blockNum)
            val newLH  = Math.min(curLH, meta.blockNum)
            val newLVH = curLVH.updated(meta.sender, meta.blockNum)
            (newHH, newLH, newLVH)
        }
        // messages higher then highest latest message are outside the view
        (highExcess, withoutHighExcess) = heightMap.partition { case (h, _) => h > highestHeight }

        // messages inside range of latest message heights might contain new messages from validator that are not seen yet
        lmRangeViews <- heightMap
                         .range(lowestHeight, highestHeight + 1)
                         .toStream
                         .traverse {
                           case (h, messages) =>
                             for {
                               lms <- messages.toStream
                                       .traverse(this.lookupUnsafe)
                                       .map(_.map(m => (m.blockHash, m.sender)))
                               (inView, excess) = lms.partition {
                                 case (_, sender) =>
                                   // remove message if target view is not aware of validator,
                                   // or latest message in full view is newer then target
                                   val senderSeen = lmPerValidatorHeight.contains(sender)
                                   senderSeen && h <= lmPerValidatorHeight(sender)
                               }
                               r = HeightView(
                                 h,
                                 excess = excess.map { case (hash, _) => hash }.toIterator,
                                 inView = inView.map { case (hash, _) => hash }.toIterator
                               )
                             } yield r
                         }
                         .map(_.toSet)
        excessMessages = lmRangeViews.flatMap(_.excess) ++ highExcess.values.flatten

        // check clause 3
        fullLMMetas <- this.latestMessages
        // senders that are in the full dag but not seen by view that is being created
        unseenSenders = fullLMMetas.filter { case (_, meta) => meta.blockNum != 0 }.keySet diff targetLatestMessages.keySet
        unseenSendersExcess <- Stream
                              // gather all messages of unseen validators
                                .fromIterator(unseenSenders.toIterator)
                                .map(fullLMMetas(_).blockHash)
                                .flatMap { h =>
                                  Stream(h) ++ this.selfJustificationChain(h).map(_.latestBlockHash)
                                }
                                // if message is already removed no need to process it again
                                .filterNot(excessMessages.contains)
                                .evalMap(this.lookupUnsafe)
                                // the lowest message in the DAG is always genesis - never remove genesis
                                .filterNot(_.blockNum == heightMap.keySet.min)
                                .map(meta => meta.blockNum -> meta.blockHash)
                                // accumulate messages into low excess map
                                .mapAccumulate(Map.empty[Long, Set[BlockHash]]) {
                                  case (acc, (height, hash)) =>
                                    val newVal = acc.getOrElse(height, Set.empty[BlockHash]) + hash
                                    (acc.updated(height, newVal), hash)
                                }
                                .map { case (acc, _) => acc }
                                .compile
                                .last
                                .map(_.getOrElse(Map.empty))
        // new truncated values
        truncatedDagSet     = this.dagSet diff excessMessages diff unseenSendersExcess.values.flatten.toSet
        truncatedInvalidSet = this.invalidBlocksSet.filter(m => dagSet.contains(m.blockHash))
        withoutRangeExcess = lmRangeViews.foldLeft(withoutHighExcess)(
          (acc, heightView) => acc.updated(heightView.height, heightView.inView.toSet)
        )
        truncatedHeightMap = unseenSendersExcess.foldLeft(withoutRangeExcess) {
          case (acc, (height, excess)) => acc.updated(height, acc(height) -- excess)
        }

        view = this.copy(
          dagSet = truncatedDagSet,
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
