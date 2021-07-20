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
import coop.rchain.blockstorage.state.CasperST
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

  private case class KeyValueDagRepresentation(st: CasperST[BlockHash])
      extends BlockDagRepresentation[F] {

    def lookup(blockHash: BlockHash): F[Option[BlockMetadata]] =
      if (st.dagSet.contains(blockHash)) blockMetadataIndex.get(blockHash)
      else none[BlockMetadata].pure[F]

    def contains(blockHash: BlockHash): F[Boolean] =
      (blockHash.size == BlockHash.Length && st.dagSet.contains(blockHash)).pure[F]

    def children(blockHash: BlockHash): F[Option[Set[BlockHash]]] =
      st.childMap.get(blockHash).map(_ intersect st.dagSet).pure[F]

    def latestMessageHash(validator: Validator): F[Option[BlockHash]] =
      st.latestMessagesMap.get(validator).pure[F]

    def latestMessageHashes: F[Map[Validator, BlockHash]] = st.latestMessagesMap.pure[F]

    def invalidBlocks: F[Set[BlockMetadata]] =
      st.invalidBlocksSet.toStream.traverse(this.lookupUnsafe).map(_.toSet)

    /**
      * Truncate full DAG according to some view, defined by latest messages.
      */
    override def truncate(
        targetLatestMessages: Map[Validator, BlockHash],
        findLfb: Map[Validator, BlockHash] => F[BlockHash]
    ): F[BlockDagRepresentation[F]] = {
      val lmAll                     = st.latestMessagesMap.values.toSet
      val lmSeen                    = targetLatestMessages.values.toSet
      val targetEqualLatestView     = lmAll == lmSeen
      val knownSenders              = st.latestMessagesMap.keySet
      val seenSenders               = targetLatestMessages.keySet
      val unknownSender             = targetLatestMessages.keysIterator.find(!knownSenders.contains(_))
      def noSenderMsg(v: Validator) = s"Error when truncating the DAG: sender ${v.show} unknown."

      val doTruncate = for {
        lfb             <- findLfb(targetLatestMessages)
        lfbHeight       <- this.lookupUnsafe(lfb).map(_.blockNum)
        latestFinalized <- this.latestFinalized(lfb, seenSenders).map(_.valuesIterator.toSet)
        // for all known latest messages, collect all self justifications until first finalized message found
        toAdjust <- Stream
                     .fromIterator(lmAll.iterator)
                     .flatMap(
                       this
                       // message + all self justifications
                         .fromSenderBelowWith(_, m => (m.blockHash, m.blockNum))
                         // take while first finalized message is found
                         .takeWhile {
                           case (hash, _) => !latestFinalized.contains(hash)
                         }
                         .mapAccumulate(init = false) {
                           case (childIsSeen, (hash, height)) =>
                             val seen = childIsSeen || lmSeen.contains(hash)
                             (seen, (hash, height))
                         }
                     )
                     .compile
                     .toList

        (u, r) = toAdjust.partition { case (seen, _) => seen }

        toUnfinalize = u.map { case (_, (hash, height)) => (hash, height) }
        toRemove     = r.map { case (_, (hash, height)) => (hash, height) }

        excessSet       = toRemove.map { case (hash, _)     => hash }.toSet
        nonFinalizedSet = toUnfinalize.map { case (hash, _) => hash }.toSet

        // new truncated values
        truncatedDagSet     = st.dagSet diff excessSet
        truncatedInvalidSet = st.invalidBlocksSet.intersect(st.dagSet)
        truncatedHeightMap = toRemove.foldLeft(st.heightMap) {
          case (acc, (hash, height)) => acc.updated(height, acc(height) - hash)
        }
        truncatedFinalizesSet = st.finalizedBlocksSet diff excessSet diff nonFinalizedSet

        view = KeyValueDagRepresentation(
          st.copy(
            lastFinalizedBlock = (lfb, lfbHeight),
            finalizedBlocksSet = truncatedFinalizesSet,
            dagSet = truncatedDagSet,
            latestMessagesMap = targetLatestMessages,
            heightMap = truncatedHeightMap,
            invalidBlocksSet = truncatedInvalidSet
          )
        )

      } yield view.asInstanceOf[BlockDagRepresentation[F]]

      unknownSender match {
        case None =>
          if (targetEqualLatestView) this.asInstanceOf[BlockDagRepresentation[F]].pure
          else doTruncate
        case Some(v) => new Exception(noSenderMsg(v)).raiseError[F, BlockDagRepresentation[F]]
      }
    }

    override def lastFinalizedBlock: BlockHash = st.lastFinalizedBlock._1

    // latestBlockNumber, topoSort and lookupByDeployId are only used in BlockAPI.
    // Do they need to be part of the DAG current state or they can be moved to DAG storage directly?

    private def getMaxHeight = if (st.heightMap.nonEmpty) st.heightMap.last._1 + 1L else 0L

    def latestBlockNumber: F[Long] =
      getMaxHeight.pure[F]

    def isFinalized(blockHash: BlockHash): F[Boolean] =
      st.finalizedBlocksSet.contains(blockHash).pure[F]

    def topoSort(
        startBlockNumber: Long,
        maybeEndBlockNumber: Option[Long]
    ): F[Vector[Vector[BlockHash]]] = {
      val maxNumber   = getMaxHeight
      val startNumber = Math.max(0, startBlockNumber)
      val endNumber   = maybeEndBlockNumber.map(Math.min(maxNumber, _)).getOrElse(maxNumber)
      if (startNumber >= 0 && startNumber <= endNumber) {
        Sync[F].delay(
          st.heightMap
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

    override def nonFinalizedSet: Set[BlockHash] =
      st.dagSet diff st.finalizedBlocksSet
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
      latestMessages           <- latestMessagesIndex.toMap
      dagSet                   <- blockMetadataIndex.dagSet
      childMap                 <- blockMetadataIndex.childMapData
      heightMap                <- blockMetadataIndex.heightMap
      invalidBlocks            <- invalidBlocksIndex.toMap.map(_.toSeq.map(_._1).toSet)
      lastFinalizedBlock       <- blockMetadataIndex.lastFinalizedBlock
      lastFinalizedBlockHeight <- blockMetadataIndex.getUnsafe(lastFinalizedBlock).map(_.blockNum)
      finalizedBlocksSet       <- blockMetadataIndex.finalizedBlockSet
    } yield KeyValueDagRepresentation(
      CasperST(
        Map.empty, // buffer is empty
        dagSet,
        latestMessages,
        childMap,
        heightMap,
        invalidBlocks,
        (lastFinalizedBlock, lastFinalizedBlockHeight),
        finalizedBlocksSet
      )
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
        curLfb = dag.lastFinalizedBlock
        lateUpdate <- List(curLfb, directlyFinalizedHash)
                       .traverse(dag.lookupUnsafe(_).map(_.blockNum))
                       .map {
                         case List(c, n) => c >= n
                       }
        _ <- if (lateUpdate) ().pure
            else {
              val errMsg =
                s"Attempting to finalize nonexistent hash ${PrettyPrinter.buildString(directlyFinalizedHash)}."
              for {
                _ <- dag
                      .contains(directlyFinalizedHash)
                      .ifM(().pure, new Exception(errMsg).raiseError)
                // all non finalized ancestors should be finalized as well (indirectly)
                indirectlyFinalized <- dag.ancestors(
                                        List(directlyFinalizedHash),
                                        dag.isFinalized(_).not
                                      )
                // invoke effects
                _ <- finalizationEffect(indirectlyFinalized + directlyFinalizedHash)
                // persist finalization
                _ <- blockMetadataIndex.recordFinalized(directlyFinalizedHash, indirectlyFinalized)
              } yield ()
            }

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
