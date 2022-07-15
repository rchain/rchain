package coop.rchain.casper.dag

import cats.Monad
import cats.effect.Concurrent
import cats.effect.concurrent.{Ref, Semaphore}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage._
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.dag.BlockMetadataStore.BlockMetadataStore
import coop.rchain.blockstorage.dag._
import coop.rchain.blockstorage.dag.codecs._
import coop.rchain.casper.dag.BlockDagKeyValueStorage._
import coop.rchain.casper.protocol.{BlockMessage, DeployData}
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.{MultiParentCasper, PrettyPrinter}
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, MetricsSemaphore}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.Validator.Validator
import coop.rchain.models.syntax._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.syntax._
import coop.rchain.shared.{Log, LogSource}
import coop.rchain.store.{KeyValueStoreManager, KeyValueTypedStore}
import fs2.Stream

import scala.collection.concurrent.TrieMap

final class BlockDagKeyValueStorage[F[_]: Concurrent: Log] private (
    representationState: Ref[F, DagRepresentation],
    lock: Semaphore[F],
    blockMetadataIndex: BlockMetadataStore[F],
    deployIndex: KeyValueTypedStore[F, DeployId, BlockHash],
    deployStore: KeyValueTypedStore[F, DeployId, Signed[DeployData]]
) extends BlockDagStorage[F] {

  def getRepresentation: F[DagRepresentation] = representationState.get

  override def insertNew(
      blockMetadata: BlockMetadata,
      block: BlockMessage
  ): F[DagRepresentation] = {
    val logAlreadyStored =
      Log[F].warn(s"Block ${PrettyPrinter.buildString(block, short = true)} is already stored.")

    def doInsert: F[Unit] =
      for {
        // Add block metadata
        _ <- blockMetadataIndex.add(blockMetadata)

        _ <- Log[F].info(s"Block ${PrettyPrinter.buildString(block, short = true)} added to DAG.")

        // Add deploys to deploy index storage
        deployHashes = block.state.deploys.map(_.deploy.sig)
        _            <- deployIndex.put(deployHashes.map(_ -> block.blockHash))
      } yield ()

    lock.withPermit(
      blockMetadataIndex
        .contains(blockMetadata.blockHash)
        .ifM(logAlreadyStored, doInsert) >> {
        for {
          // Take current DAG state / view of the DAG
          dagSet    <- blockMetadataIndex.dagSet
          childMap  <- blockMetadataIndex.childMapData
          heightMap <- blockMetadataIndex.heightMap
          dag <- representationState.updateAndGet {
                  dr =>
                    // Update DAG messages state
                    val dagMsgSt       = dr.dagMessageState
                    val msg            = messageFromBlockMetadata(blockMetadata, dagMsgSt.msgMap)
                    val newDagMsgState = dagMsgSt.insertMsg(msg)

                    // Update fringes state
                    val fringeStateHash       = blockMetadata.fringeStateHash.toBlake2b256Hash
                    val fringeRejectedDeploys = block.rejectedDeploys.toSet
                    val fringeStateRecord     = (fringeStateHash, fringeRejectedDeploys)
                    val newFringes            = dr.fringeStates + ((msg.fringe, fringeStateRecord))

                    // Updated DagRepresentation
                    dr.copy(
                      dagSet,
                      childMap,
                      heightMap,
                      newDagMsgState,
                      fringeStates = newFringes
                    )
                }

          _ <- removeExpiredFromPool(deployStore, dag).map(
                _.map((_, ())).map((expiredMap.update _).tupled)
              )
        } yield dag
      }
    )
  }

  // TODO: legacy function, used only in tests, it should be removed when tests are fixed
  def insert(block: BlockMessage, invalid: Boolean, approved: Boolean): F[DagRepresentation] =
    for {
      fringeWithState <- if (approved) {
                          (Set(block.blockHash), block.postStateHash).pure[F]
                        } else {
                          for {
                            dag       <- getRepresentation
                            dagMsgSt  = dag.dagMessageState
                            finalizer = Finalizer(dagMsgSt.msgMap)
                            parents   = block.justifications.map(dagMsgSt.msgMap).toSet
                            fringe    = finalizer.latestFringe(parents).map(_.id)
                            (fringeState, _) = if (fringe.isEmpty)
                              (
                                RuntimeManager.emptyStateHashFixed.toBlake2b256Hash,
                                Set[ByteString]()
                              )
                            else
                              dag.fringeStates(fringe)
                          } yield (fringe, fringeState.toByteString)
                        }

      (fringe, fringeState) = fringeWithState
      bmd = BlockMetadata
        .fromBlock(block)
        .copy(invalid = invalid, fringe = fringe.toList, fringeStateHash = fringeState)

      result <- insertNew(bmd, block)
    } yield result

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
          dagSet    <- metadata.dagSet
          childMap  <- metadata.childMapData
          heightMap <- metadata.heightMap

          // Fill message map from BlockMetadata
          // TODO: include only non-finalized block
          dmsSt <- Ref.of(DagMessageState[BlockHash, Validator]())
          fsSt  <- Ref.of(Map[Set[BlockHash], (Blake2b256Hash, Set[ByteString])]())
          initMsgMapJob = Stream.fromIterator(heightMap.values.flatten.iterator).evalMap { hash =>
            for {
              ds     <- dmsSt.get
              fs     <- fsSt.get
              msgMap = ds.msgMap
              updateMessage = for {
                block <- metadata.getUnsafe(hash)
                msg   = messageFromBlockMetadata(block, msgMap)
                newDs = ds.insertMsg(msg)

                // TODO: fill rejected deploys! should we store it in BlockMetadata?
                fringeStateHash = block.fringeStateHash.toBlake2b256Hash
                newFs           = fs + ((block.fringe.toSet, (fringeStateHash, Set[ByteString]())))
                _               <- dmsSt.set(newDs)
                _               <- fsSt.set(newFs)
              } yield ()

              // Check if already created
              _ <- updateMessage.unlessA(msgMap.contains(hash))
            } yield ()
          }

          // Initialize DagMessageState
          _            <- initMsgMapJob.compile.drain
          dagMsgsState <- dmsSt.get
          fringeStates <- fsSt.get
        } yield DagRepresentation(dagSet, childMap, heightMap, dagMsgsState, fringeStates)
      }
      stRef <- Ref.of[F, DagRepresentation](initST)
    } yield new BlockDagKeyValueStorage[F](
      stRef,
      lock,
      stores.metadata,
      stores.deploys,
      stores.deployPool
    )

  private def messageFromBlockMetadata(
      block: BlockMetadata,
      msgMap: Map[BlockHash, Message[BlockHash, Validator]]
  ) = {
    val parents = block.justifications.toSet
    // Seen messages are all seen from justifications combined
    val seen = parents.map(msgMap).flatMap(_.seen) + block.blockHash
    Message(
      id = block.blockHash,
      height = block.blockNum,
      sender = block.sender,
      senderSeq = block.seqNum,
      bondsMap = block.bondsMap,
      parents = parents,
      fringe = block.fringe.toSet,
      seen = seen
    )
  }

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
