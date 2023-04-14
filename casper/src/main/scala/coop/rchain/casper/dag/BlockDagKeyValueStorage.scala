package coop.rchain.casper.dag

import cats.effect.Async
import cats.syntax.all._
import cats.{Monad, Show}
import coop.rchain.blockstorage._
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.dag.BlockMetadataStore.BlockMetadataStore
import coop.rchain.blockstorage.dag._
import coop.rchain.blockstorage.dag.codecs._
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.dag.BlockDagKeyValueStorage._
import coop.rchain.casper.merging.BlockIndex
import coop.rchain.casper.protocol.{BlockMessage, DeployData}
import coop.rchain.casper.{MultiParentCasper, PrettyPrinter}
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, MetricsSemaphore}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.syntax._
import coop.rchain.models.{BlockMetadata, FringeData}
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.hashing.Blake2b256Hash.codecBlake2b256Hash
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import coop.rchain.store.{KeyValueStoreManager, KeyValueTypedStore}
import fs2.Stream

import scala.collection.concurrent.TrieMap
import cats.effect.Ref
import cats.effect.std.Semaphore

final class BlockDagKeyValueStorage[F[_]: Async: Log] private (
    representationState: Ref[F, DagRepresentation],
    lock: Semaphore[F],
    blockMetadataIndex: BlockMetadataStore[F],
    fringeDataStore: KeyValueTypedStore[F, Blake2b256Hash, FringeData],
    deployIndex: KeyValueTypedStore[F, DeployId, BlockHash],
    deployStore: KeyValueTypedStore[F, DeployId, Signed[DeployData]]
) extends BlockDagStorage[F] {

  def getRepresentation: F[DagRepresentation] = representationState.get

  override def insert(
      blockMetadata: BlockMetadata,
      block: BlockMessage
  ): F[Unit] = {
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

        dagState <- representationState.get.map(_.dagMessageState)

        // Store fringe data
        fringeHash = FringeData.fringeHash(blockMetadata.fringe)
        // Calculate blocks included in the fringe
        justificationsMsgs = blockMetadata.justifications.map(dagState.msgMap)
        prevFringeMsgs     = dagState.msgMap.latestFringe(justificationsMsgs)
        fringeMsgs         = blockMetadata.fringe.map(dagState.msgMap)
        fringeDiff         = fringeMsgs.flatMap(_.seen) -- prevFringeMsgs.flatMap(_.seen)

        // Fringe data object to store
        fringeData = FringeData(
          fringeHash,
          fringe = blockMetadata.fringe,
          fringeDiff = fringeDiff,
          stateHash = blockMetadata.fringeStateHash.toBlake2b256Hash,
          rejectedDeploys = block.rejectedDeploys,
          rejectedBlocks = block.rejectedBlocks,
          rejectedSenders = block.rejectedSenders
        )
        // Save to fringe data store
        _ <- fringeDataStore.put(fringeHash, fringeData)

        // Update block metadata members of finalized fringe
        fringeDiffMetas        <- fringeDiff.toList.traverse(blockMetadataIndex.getUnsafe)
        fringeDiffMetasUpdated = fringeDiffMetas.map(_.copy(memberOfFringe = fringeHash.some))
        _                      <- fringeDiffMetasUpdated.traverse(blockMetadataIndex.add)

        // Take current DAG state / view of the DAG
        dagSet    <- blockMetadataIndex.dagSet
        childMap  <- blockMetadataIndex.childMapData
        heightMap <- blockMetadataIndex.heightMap
        dag <- representationState.updateAndGet { dr =>
                // Update DAG messages state
                val dagMsgSt       = dr.dagMessageState
                val msg            = messageFromBlockMetadata(blockMetadata, dagMsgSt.msgMap)
                val newDagMsgState = dagMsgSt.insertMsg(msg)

                // Update fringe data cache
                // TODO: remove out of reach records (not needed for further finalization)
                val newFringes = dr.fringeStates + ((msg.fringe, fringeData))

                // Updated DagRepresentation
                dr.copy(
                  dagSet,
                  childMap,
                  heightMap,
                  newDagMsgState,
                  fringeStates = newFringes
                )
              }

        // attempt to prune only when sender is the only outsider
        lowestFringe            = dagState.msgMap.lowestFringe(dagState.latestMsgs).map(_.id)
        outsiders               = dagState.latestMsgs.filter(_.fringe == lowestFringe).map(_.sender)
        senderIsTheOnlyOutsider = outsiders == Set(blockMetadata.sender)
        shouldPrune             = fringeDiff.nonEmpty && senderIsTheOnlyOutsider
        _                       <- pruneDiff(dag.dagMessageState, dagState, childMap).whenA(shouldPrune)

        _ <- removeExpiredFromPool(deployStore, dag).map(
              _.map((_, ())).map((expiredMap.update _).tupled)
            )
      } yield ()

    lock.permit.use { _ =>
      blockMetadataIndex
        .contains(blockMetadata.blockHash)
        .ifM(logAlreadyStored, doInsert)
    }
  }

  /**
    * Fringe messages below which (+ messages of the fringe) can be pruned since they are not
    * required for processing of any future message.
    */
  def dbPruneFringe(
      dbState: DagMessageState[BlockHash, Validator],
      childMap: Map[BlockHash, Set[BlockHash]]
  ): Set[Message[BlockHash, Validator]] = {
    val lowestFringe = dbState.msgMap.lowestFringe(dbState.latestMsgs).map(_.id)
    dbState.msgMap.pruneFringe(lowestFringe, childMap)
  }

  /**
    * Prune database.
    * Remove data that is not required for processing any future block.
    * TODO for now its just clean merging index cache, but can be used to prune the whole DB.
    * `Diff` here is because data pruned is what is not required in new state compared to current state.
    */
  def pruneDiff(
      newState: DagMessageState[BlockHash, Validator],
      curState: DagMessageState[BlockHash, Validator],
      childMap: Map[BlockHash, Set[BlockHash]]
  ): F[Unit] = {
    val newLPF  = dbPruneFringe(newState, childMap)
    val curLPF  = dbPruneFringe(curState, childMap)
    val toPrune = (newState.msgMap.between(newLPF, curLPF) ++ curLPF).map(_.id)

    toPrune.toList.foreach(BlockIndex.cache.remove)
    Log[F].info(s"Pruned ${toPrune.size} merging indices, new size: ${BlockIndex.cache.size}")
  }

  override def lookup(blockHash: BlockHash): F[Option[BlockMetadata]] =
    blockMetadataIndex.get(blockHash)

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
      fringeDataDb: KeyValueTypedStore[F, Blake2b256Hash, FringeData],
      deploys: KeyValueTypedStore[F, DeployId, BlockHash],
      deployPool: KeyValueTypedStore[F, DeployId, Signed[DeployData]]
  )

  private def createStores[F[_]: Async: Log: Metrics](kvm: KeyValueStoreManager[F]) = {
    implicit val kvm_ = kvm
    for {
      // Block metadata map
      blockMetadataDb <- KeyValueStoreManager[F].database[BlockHash, BlockMetadata](
                          "block-metadata",
                          codecBlockHash,
                          codecBlockMetadata
                        )
      blockMetadataStore <- BlockMetadataStore[F](blockMetadataDb)

      // Fringe data map
      fringeDataDb <- KeyValueStoreManager[F].database[Blake2b256Hash, FringeData](
                       "fringe-data",
                       codecBlake2b256Hash,
                       codecFringeData
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
      fringeDataDb,
      deployIndexDb,
      deployPoolDb
    )
  }

  def create[F[_]: Async: Log: Metrics](
      kvm: KeyValueStoreManager[F]
  ): F[BlockDagKeyValueStorage[F]] =
    for {
      lock   <- MetricsSemaphore.single[F]
      stores <- createStores(kvm)
      initST <- {
        val metadata = stores.metadata
        for {
          // Take current DAG state / view of the DAG
          dagSet    <- metadata.dagSet
          childMap  <- metadata.childMapData
          heightMap <- metadata.heightMap

          // Fill message map from BlockMetadata
          // TODO: include only non-finalized block
          dmsSt <- Ref.of(DagMessageState[BlockHash, Validator]())
          fsSt  <- Ref.of(Map[Set[BlockHash], FringeData]())
          i     = heightMap.values.flatten.iterator
          initMsgMapJob = Stream.fromIterator(i, 1).evalMap { hash =>
            for {
              ds     <- dmsSt.get
              fs     <- fsSt.get
              msgMap = ds.msgMap
              updateMessage = for {
                block <- metadata.getUnsafe(hash)
                msg   = messageFromBlockMetadata(block, msgMap)
                newDs = ds.insertMsg(msg)

                fringeDataCached = fs.contains(msg.fringe)
                newFs <- if (!fringeDataCached) {
                          implicit val showHash = Show.show[Blake2b256Hash](_.bytes.toHex)
                          val fringeHash        = FringeData.fringeHash(msg.fringe)
                          stores.fringeDataDb
                            .getUnsafe(fringeHash)
                            .map(fd => fs + ((msg.fringe, fd)))
                        } else {
                          fs.pure[F]
                        }

                _ <- dmsSt.set(newDs)
                _ <- fsSt.set(newFs)
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
      stores.fringeDataDb,
      stores.deploys,
      stores.deployPool
    )

  private def messageFromBlockMetadata(
      block: BlockMetadata,
      msgMap: Map[BlockHash, Message[BlockHash, Validator]]
  ) = {
    val parents = block.justifications
    // Seen messages are all seen from justifications combined
    val seen = parents.map(msgMap).flatMap(_.seen) + block.blockHash
    Message(
      id = block.blockHash,
      height = block.blockNum,
      sender = block.sender,
      senderSeq = block.seqNum,
      bondsMap = block.bondsMap,
      parents = parents,
      fringe = block.fringe,
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
