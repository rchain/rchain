package coop.rchain.casper.dag

import cats.Monad
import cats.effect.{Concurrent, Sync}
import cats.effect.concurrent.{Ref, Semaphore}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage._
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.dag.BlockMetadataStore.BlockMetadataStore
import coop.rchain.blockstorage.dag.codecs._
import coop.rchain.blockstorage.dag._
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.{MultiParentCasper, PrettyPrinter}
import coop.rchain.casper.protocol.{BlockMessage, DeployData}
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, MetricsSemaphore}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.{BlockHash, BlockMetadata, Validator}
import coop.rchain.shared.syntax._
import coop.rchain.casper.dag.BlockDagKeyValueStorage._
import coop.rchain.models.syntax._
import coop.rchain.rholang.interpreter.EvaluateResult
import coop.rchain.shared.{Log, LogSource}
import coop.rchain.store.{KeyValueStoreManager, KeyValueTypedStore}

import scala.collection.concurrent.TrieMap

final class BlockDagKeyValueStorage[F[_]: Concurrent: Log] private (
    representationState: Ref[F, DagRepresentation],
    lock: Semaphore[F],
    latestMessagesIndex: KeyValueTypedStore[F, Validator, BlockHash],
    blockMetadataIndex: BlockMetadataStore[F],
    deployIndex: KeyValueTypedStore[F, DeployId, BlockHash],
    deployStore: KeyValueTypedStore[F, DeployId, Signed[DeployData]],
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

    // Empty sender is valid for genesis
    val senderIsEmpty          = block.sender == ByteString.EMPTY
    val senderHasInvalidFormat = !senderIsEmpty && (block.sender.size() != Validator.Length)
    val sendersNewLM           = (block.sender, block.blockHash)

    val logAlreadyStored =
      Log[F].warn(s"Block ${PrettyPrinter.buildString(block, short = true)} is already stored.")

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

    def doInsert: F[Unit] = {
      val blockMetadata      = BlockMetadata.fromBlock(block, invalid)
      val blockHashIsInvalid = !(block.blockHash.size == BlockHash.Length)

      for {
        // TODO: remove these checks, block hash and sender should be checked when block is received
        // Basic validation of input hash values
        _ <- BlockSenderIsMalformed(block).raiseError[F, Unit].whenA(senderHasInvalidFormat)
        _ <- new Exception(
              s"Block hash (${PrettyPrinter.buildString(block.blockHash)}) is not correct length."
            ).raiseError[F, Unit].whenA(blockHashIsInvalid)
        _ <- new Exception(
              s"Block ${PrettyPrinter.buildString(block, short = true)} sender is empty."
            ).raiseError[F, Unit].whenA(senderIsEmpty)

        // Add block metadata
        _ <- blockMetadataIndex.add(blockMetadata)

        // Add deploys to deploy index storage
        deployHashes = block.body.deploys.map(_.deploy.sig)
        _            <- deployIndex.put(deployHashes.map(_ -> block.blockHash))

        // Update invalid index
        _ <- invalidBlocksIndex.put(blockMetadata.blockHash, blockMetadata).whenA(invalid)

        // Resolve if block should be added as the latest message for the block sender
        isLatestFromSender <- shouldAddAsLatest
        newLatestToAdd     = if (isLatestFromSender) Map(sendersNewLM) else Map[Validator, BlockHash]()

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
        for {
          // Take current DAG state / view of the DAG
          latestMessages     <- latestMessagesIndex.toMap
          dagSet             <- blockMetadataIndex.dagSet
          childMap           <- blockMetadataIndex.childMapData
          heightMap          <- blockMetadataIndex.heightMap
          invalidBlocks      <- invalidBlocksIndex.toMap.map(_.toSeq.map(_._2).toSet)
          lastFinalizedBlock <- blockMetadataIndex.lastFinalizedBlock
          finalizedBlocksSet <- blockMetadataIndex.finalizedBlockSet
          dag <- representationState.updateAndGet {
                  dr =>
                    val dagMsgSt = dr.dagMessageState

                    // Update DAG messages state
                    // TODO: temporary don't expect that all justifications are available in msgMap to satisfy the failing tests
                    //  - with multi-parent finalizer all messages should be available
                    val justificationsOpt =
                      block.justifications
                        .map(dagMsgSt.msgMap.get)
                        .sequence
                        .map(_.toSet)

                    val newDagMsgState = justificationsOpt
                      .map { justifications =>
                        val newMsg = dagMsgSt.createMessage(
                          block.blockHash,
                          block.blockNumber,
                          block.sender,
                          block.seqNum,
                          block.bonds,
                          justifications
                        )
                        dagMsgSt.insertMsg(newMsg)
                      }
                      .getOrElse(dagMsgSt)

                    // Updated DagRepresentation
                    dr.copy(
                      dagSet,
                      latestMessages,
                      childMap,
                      heightMap,
                      invalidBlocks.map(_.blockHash),
                      lastFinalizedBlock,
                      finalizedBlocksSet,
                      newDagMsgState
                    )
                }
          _ <- removeExpiredFromPool(deployStore, dag).map(
                _.map((_, ())).map((expiredMap.update _).tupled)
              )
        } yield dag
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

  override def addDeploy(d: Signed[DeployData]): F[Unit] = deployStore.put(d.sig, d)

  override def pooledDeploys: F[Map[DeployId, Signed[DeployData]]] = deployStore.toMap

  override def containsDeployInPool(deployId: DeployId): F[Boolean] = deployStore.contains(deployId)

  // Map of deploys being executed and execution results
  private val expiredMap = TrieMap.empty[DeployId, Unit]
}

object BlockDagKeyValueStorage {
  implicit private val BlockDagKeyValueStorage_FromFileMetricsSource: Source =
    Metrics.Source(BlockStorageMetricsSource, "dag-key-value-store")

  private final case class DagStores[F[_]](
      metadata: BlockMetadataStore[F],
      metadataDb: KeyValueTypedStore[F, BlockHash, BlockMetadata],
      latestMessages: KeyValueTypedStore[F, Validator, BlockHash],
      invalidBlocks: KeyValueTypedStore[F, BlockHash, BlockMetadata],
      deploys: KeyValueTypedStore[F, DeployId, BlockHash],
      deployPool: KeyValueTypedStore[F, DeployId, Signed[DeployData]]
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
      // Deploy pool storage
      deployPoolDb <- KeyValueStoreManager[F].database[DeployId, Signed[DeployData]](
                       "deploy-pool",
                       codecByteString,
                       codecSignedDeployData
                     )
    } yield DagStores(
      blockMetadataStore,
      blockMetadataDb,
      latestMessagesDb,
      invalidBlocksDb,
      deployIndexDb,
      deployPoolDb
    )
  }

  def create[F[_]: Concurrent: Log: Metrics](
      kvm: KeyValueStoreManager[F]
  ): F[BlockDagKeyValueStorage[F]] =
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
      stores.deployPool,
      stores.invalidBlocks
    )

  def removeExpiredFromPool[F[_]: Monad](
      deployStore: KeyValueTypedStore[F, DeployId, Signed[DeployData]],
      dag: DagRepresentation
  ): F[List[DeployId]] = {
    val expiredF = deployStore
      .collect {
        case (_, v) =>
          val d       = v()
          val expired = dag.latestBlockNumber - d.data.validAfterBlockNumber > MultiParentCasper.deployLifespan
          expired.guard[Option].as(d)
      }
      .map(_.flatten.toList)
    expiredF.flatMap { v =>
      val sigs = v.map(_.sig)
      deployStore.delete(sigs).as(sigs)
    }
  }
}
