package coop.rchain.casper.dag

import cats.effect.Concurrent
import cats.effect.concurrent.{Ref, Semaphore}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage._
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.dag.BlockMetadataStore.BlockMetadataStore
import coop.rchain.blockstorage.dag.codecs._
import coop.rchain.blockstorage.dag.{BlockDagStorage, BlockMetadataStore, DagRepresentation}
import coop.rchain.blockstorage.syntax._
import coop.rchain.blockstorage.util.BlockMessageUtil._
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, MetricsSemaphore}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.{BlockHash, BlockMetadata, Validator}
import coop.rchain.shared.syntax._
import coop.rchain.shared.{Log, LogSource}
import coop.rchain.store.{KeyValueStoreManager, KeyValueTypedStore}

final class BlockDagKeyValueStorage[F[_]: Concurrent: Log] private (
    representationState: Ref[F, DagRepresentation],
    lock: Semaphore[F],
    latestMessagesIndex: KeyValueTypedStore[F, Validator, BlockHash],
    blockMetadataIndex: BlockMetadataStore[F],
    deployIndex: KeyValueTypedStore[F, DeployId, BlockHash],
    invalidBlocksIndex: KeyValueTypedStore[F, BlockHash, BlockMetadata]
) extends BlockDagStorage[F] {
  implicit private val logSource: LogSource = LogSource(BlockDagKeyValueStorage.getClass)

  def getRepresentation: F[DagRepresentation] = representationState.get

  def insert(
      block: BlockMessage,
      invalid: Boolean,
      approved: Boolean
  ): F[DagRepresentation] = {
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
        .ifM(logAlreadyStored, doInsert) >> {
        // TODO BlockDagStore inset method should be rewritten to only save to disk,
        //  all logic should be done on the state.
        //  This update of the state as a result of insert method should be removed.
        val representation =
          for {
            // Take current DAG state / view of the DAG
            latestMessages     <- latestMessagesIndex.toMap
            dagSet             <- blockMetadataIndex.dagSet
            childMap           <- blockMetadataIndex.childMapData
            heightMap          <- blockMetadataIndex.heightMap
            invalidBlocks      <- invalidBlocksIndex.toMap.map(_.toSeq.map(_._2).toSet)
            lastFinalizedBlock <- blockMetadataIndex.lastFinalizedBlock
            finalizedBlocksSet <- blockMetadataIndex.finalizedBlockSet
          } yield DagRepresentation(
            dagSet,
            latestMessages,
            childMap,
            heightMap,
            invalidBlocks.map(_.blockHash),
            lastFinalizedBlock,
            finalizedBlocksSet
          )
        representation.flatTap(representationState.set)
      }
    )
  }

  /** Record that some hash is directly finalized (detected by finalizer and becomes LFB). */
  def recordDirectlyFinalized(
      directlyFinalizedHash: BlockHash,
      finalizationEffect: Set[BlockHash] => F[Unit]
  ): F[Unit] = {
    implicit val bds = this
    // Lock here is a safeguard for persisting changes in BlockMetadataIndex which can happen concurrently when
    // blocks are replayed in parallel
    lock.withPermit(
      for {
        dag    <- getRepresentation
        errMsg = s"Attempting to finalize nonexistent hash ${PrettyPrinter.buildString(directlyFinalizedHash)}."
        _      <- new Exception(errMsg).raiseError.unlessA(dag.contains(directlyFinalizedHash))
        // all non finalized ancestors should be finalized as well (indirectly)
        indirectlyFinalized <- dag.ancestors(directlyFinalizedHash, dag.isFinalized(_).pure.not)
        // invoke effects
        _ <- finalizationEffect(indirectlyFinalized + directlyFinalizedHash)
        // persist finalization
        _ <- blockMetadataIndex.recordFinalized(directlyFinalizedHash, indirectlyFinalized)

        // TODO: temp code to satisfy tests
        _ <- representationState.update { dag =>
              val newFinalizedSet = dag.finalizedBlocksSet ++ indirectlyFinalized + directlyFinalizedHash
              dag.copy(
                finalizedBlocksSet = newFinalizedSet,
                lastFinalizedBlockHash = directlyFinalizedHash.some
              )
            }
      } yield ()
    )
  }

  override def lookup(
      blockHash: BlockHash
  ): F[Option[BlockMetadata]] = blockMetadataIndex.get(blockHash)

  override def lookupByDeployId(deployId: DeployId): F[Option[BlockHash]] =
    deployIndex.get1(deployId)
}

object BlockDagKeyValueStorage {
  implicit private val BlockDagKeyValueStorage_FromFileMetricsSource: Source =
    Metrics.Source(BlockStorageMetricsSource, "dag-key-value-store")

  private final case class DagStores[F[_]](
      metadata: BlockMetadataStore[F],
      metadataDb: KeyValueTypedStore[F, BlockHash, BlockMetadata],
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
                        codecByteString,
                        codecBlockHash
                      )

    } yield DagStores(
      blockMetadataStore,
      blockMetadataDb,
      latestMessagesDb,
      invalidBlocksDb,
      deployIndexDb
    )
  }

  def create[F[_]: Concurrent: Log: Metrics](kvm: KeyValueStoreManager[F]): F[BlockDagStorage[F]] =
    for {
      lock   <- MetricsSemaphore.single[F]
      stores <- createStores(kvm)
      initST <- {
        import stores._
        for {
          // Take current DAG state / view of the DAG
          latestMessages     <- latestMessages.toMap
          dagSet             <- metadata.dagSet
          childMap           <- metadata.childMapData
          heightMap          <- metadata.heightMap
          invalidBlocks      <- invalidBlocks.toMap.map(_.toSeq.map(_._2).toSet)
          lastFinalizedBlock <- metadata.lastFinalizedBlock
          finalizedBlocksSet <- metadata.finalizedBlockSet
        } yield DagRepresentation(
          dagSet,
          latestMessages,
          childMap,
          heightMap,
          invalidBlocks.map(_.blockHash),
          lastFinalizedBlock,
          finalizedBlocksSet
        )
      }
      stRef <- Ref.of[F, DagRepresentation](initST)
    } yield new BlockDagKeyValueStorage[F](
      stRef,
      lock,
      stores.latestMessages,
      stores.metadata,
      stores.deploys,
      stores.invalidBlocks
    )
}
