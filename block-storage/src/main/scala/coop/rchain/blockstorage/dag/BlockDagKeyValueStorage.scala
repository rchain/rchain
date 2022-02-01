package coop.rchain.blockstorage.dag

import cats.{Id, Show}
import cats.data.OptionT
import cats.effect.concurrent.{Deferred, Ref, Semaphore}
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage._
import coop.rchain.blockstorage.dag.BlockDagStorage._
import coop.rchain.blockstorage.dag.BlockMetadataStore.BlockMetadataStore
import coop.rchain.blockstorage.dag.DeployChainSetCasper._
import coop.rchain.blockstorage.dag.EquivocationTrackerStore.EquivocationTrackerStore
import coop.rchain.blockstorage.dag.codecs.{codecDeployChain, _}
import coop.rchain.blockstorage.dag.state.BlockDagRepresentationState
import coop.rchain.blockstorage.dag.state.BlockDagRepresentationState.BlockDagFinalizationState
import coop.rchain.blockstorage.syntax._
import coop.rchain.blockstorage.util.BlockMessageUtil.{bonds, _}
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.protocol.{BlockMessage, Bond, DeployChain, Justification}
import coop.rchain.blockstorage.casper.Casper
import coop.rchain.blockstorage.casper.Casper.FinalizationFringe
import coop.rchain.blockstorage.casper.ConflictsResolver.ConflictResolution
import coop.rchain.casper.pcasper.Fringe.{Fringe, LazyReconciler}
import coop.rchain.casper.pcasper.{Finalizer, PCasper}
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, MetricsSemaphore}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.EquivocationRecord.SequenceNumber
import coop.rchain.models.Validator.Validator
import coop.rchain.models.block.StateHash.StateHash
import coop.rchain.models.{BlockHash, BlockMetadata, EquivocationRecord, Validator}
import coop.rchain.shared.syntax._
import coop.rchain.shared.{Base16, Log, LogSource}
import coop.rchain.store.{KeyValueStoreManager, KeyValueTypedStore}
import coop.rchain.models.syntax._
import coop.rchain.rspace.hashing.Blake2b256Hash

import scala.collection.immutable.SortedMap
import scala.collection.mutable

final class BlockDagKeyValueStorage[F[_]: Concurrent: Log] private (
    lock: Semaphore[F],
    latestMessagesIndex: KeyValueTypedStore[F, Validator, BlockHash],
    blockMetadataIndex: BlockMetadataStore[F],
    deployIndex: KeyValueTypedStore[F, DeployId, BlockHash],
    invalidBlocksIndex: KeyValueTypedStore[F, BlockHash, BlockMetadata],
    equivocationTrackerIndex: EquivocationTrackerStore[F],
    acceptedIndex: KeyValueTypedStore[F, DeployChain, Unit],
    rejectedIndex: KeyValueTypedStore[F, DeployChain, Unit],
    latestFringesStore: KeyValueTypedStore[F, Long, DagFringe],
    finalityViewsStore: KeyValueTypedStore[F, Validator, Long]
) extends BlockDagStorage[F] {
  implicit private val logSource: LogSource = LogSource(BlockDagKeyValueStorage.getClass)

  val latestRepresentation = mutable.TreeMap.empty[Unit, BlockDagRepresentation[F]]

  private case class KeyValueDagRepresentation(st: BlockDagRepresentationState)
      extends BlockDagRepresentation[F] {

    def lookup(blockHash: BlockHash): F[Option[BlockMetadata]] =
      if (st.dagSet.contains(blockHash)) blockMetadataIndex.get(blockHash)
      else none[BlockMetadata].pure[F]

    def contains(blockHash: BlockHash): F[Boolean] =
      (blockHash.size == BlockHash.Length && st.dagSet.contains(blockHash)).pure[F]

    def children(blockHash: BlockHash): F[Option[Set[BlockHash]]] =
      st.childrenMap.get(blockHash).map(_.valuesIterator.toSet.flatten).pure[F]

    def closestChildren(blockHash: BlockHash): F[Option[Set[BlockHash]]] =
      st.childrenMap.get(blockHash).map(_.valuesIterator.map(_.head).toSet).pure[F]

    def witnesses(blockHash: BlockHash): F[Option[Set[BlockHash]]] =
      st.witnessMap.get(blockHash).map(_.values.toSet).pure[F]

    def latestMessageHash(validator: Validator): F[Option[BlockHash]] =
      st.latestMessagesMap.get(validator).pure[F]

    def latestMessageHashes: F[Map[Validator, BlockHash]] = st.latestMessagesMap.pure[F]

    def invalidBlocks: F[Set[BlockMetadata]] =
      st.invalidBlocksSet.toList.traverse(this.lookupUnsafe).map(_.toSet)

    // latestBlockNumber, topoSort and lookupByDeployId are only used in BlockAPI.
    // Do they need to be part of the DAG current state or they can be moved to DAG storage directly?

    private def getMaxHeight = if (st.heightMap.nonEmpty) st.heightMap.last._1 + 1L else 0L

    def latestBlockNumber: F[Long] = getMaxHeight.pure[F]

    def isFinalized(blockHash: BlockHash): F[Boolean] = false.pure[F]

    override def find(truncatedHash: String): F[Option[BlockHash]] = Sync[F].delay {
      if (truncatedHash.length % 2 == 0) {
        val truncatedByteString = truncatedHash.unsafeHexToByteString
        st.dagSet.find(hash => hash.startsWith(truncatedByteString))
      } else {
        // if truncatedHash is odd length string we cannot convert it to ByteString with 8 bit resolution
        // because each symbol has 4 bit resolution. Need to make a string of even length by removing the last symbol,
        // then find all the matching hashes and choose one that matches the full truncatedHash string
        val truncatedByteString = truncatedHash.dropRight(1).unsafeHexToByteString
        st.dagSet
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

    def genesis: F[BlockHash] = st.heightMap.head._2.head.pure[F]

    override def finalizationState: BlockDagFinalizationState = st.finalizationState

    override def getPureState: BlockDagRepresentationState = st

    override def finalizationFringes: SortedMap[Long, DagFringe] = st.latestFringes
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
      latestMessages <- latestMessagesIndex.toMap
      dagSet         <- blockMetadataIndex.dagSet
      childMap       <- blockMetadataIndex.childMapData
      witnessMap     <- blockMetadataIndex.witnessMap
      heightMap      <- blockMetadataIndex.heightMap
      invalidBlocks  <- invalidBlocksIndex.toMap.map(_.keySet)
      acceptedSet    <- acceptedIndex.toMap.map(_.keySet)
      rejectedSet    <- rejectedIndex.toMap.map(_.keySet)
      fringes        <- blockMetadataIndex.fringesMap
      finalityViews  <- blockMetadataIndex.finalityMap
    } yield KeyValueDagRepresentation(
      BlockDagRepresentationState(
        dagSet,
        latestMessages,
        childMap,
        witnessMap,
        heightMap,
        invalidBlocks,
        BlockDagFinalizationState(acceptedSet, rejectedSet),
        fringes,
        finalityViews
      )
    )

  def getRepresentation: F[BlockDagRepresentation[F]] =
    OptionT.fromOption(latestRepresentation.get(())).getOrElseF(representation)

  def insert(
      block: BlockMessage,
      invalid: Boolean,
      baseFringeNum: Long,
      // processing the event of finding new fringe requires more then just DAG store
      mergeFOpt: Option[
        (
            StateHash,
            Set[(BlockMetadata, Set[BlockMetadata])] // block to merge + finalized scope unseen from this block
        ) => F[(ConflictResolution[DeployChain], StateHash)]
      ]
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

    def persistFinality(cr: ConflictResolution[DeployChain], newFringe: DagFringe) =
      acceptedIndex.putIfAbsent(cr.acceptedSet.map((_, ())).toList) >>
        rejectedIndex.putIfAbsent(cr.rejectedSet.map((_, ())).toList) >>
        latestFringesStore.put(newFringe.num, newFringe) >>
        blockMetadataIndex.addFringe(newFringe)

    def doInsert: F[Unit] = {
      val initMetadata       = BlockMetadata.fromBlock(block, invalid, baseFringeNum)
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

        _  <- logEmptySender.whenA(senderIsEmpty)
        js = initMetadata.justifications.map(_.latestBlockHash)
        jsLvl2 <- js
                   .traverse(blockMetadataIndex.getUnsafe)
                   .map(_.flatMap(_.justifications.map(_.latestBlockHash)))
        parents = js diff jsLvl2
//        dag     <- getRepresentation
//        blockFinal <- FinalityState.computeMessageFinal(block, dag)(
//                       block.blockHash,
//                       block.body.state.postStateHash,
//                       block.justifications,
//                       parents,
//                       block.body.state.bonds,
//                       dag.finalizationFringes,
//                       mergeFOpt,
//                       persistFinality
//                     )

        blockMetadata = initMetadata.copy(parents = parents, finalView = Map())

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

    def updateFinality(dag: BlockDagRepresentation[F], bonds: Map[Validator, Long]) = {
      val defMergeF = (_: StateHash, _: Set[(BlockMetadata, Set[BlockMetadata])]) =>
        (ConflictResolution(Set.empty[DeployChain], Set.empty[DeployChain]), ByteString.EMPTY)
          .pure[F]
      dag.finalizationFringes.lastOption match {
        // fringe exists - try update
        case Some((latestIdx, fringe)) =>
          FinalityState.updateFinalFringe(
            dag,
            fringe,
            bonds,
            mergeFOpt.getOrElse(defMergeF),
            persistFinality
          )
        // does not exist - record block as all across fringe
        case None => {
          val f = DagFringe(
            bonds.keySet.map(v => (v, Set(block.blockHash))).toList,
            block.body.state.postStateHash,
            0L
          )
          Log[F].info(s"latestFringesStore is empty. Inserting block as a fringe.") >>
            persistFinality(ConflictResolution(Set(), Set()), f)
        }
      }
    }

    // TODO bonds for finalization should be taken from latest final state
    val bondsMap = block.body.state.bonds.map { case Bond(v, s) => v -> s }.toMap

    lock.withPermit(
      blockMetadataIndex.contains(block.blockHash).ifM(logAlreadyStored, doInsert) >>
        representation.flatMap(updateFinality(_, bondsMap)) >> representation.map { r =>
        latestRepresentation.update((), r)
        r
      }
    )
  }

  override def accessEquivocationsTracker[A](f: EquivocationsTracker[F] => F[A]): F[A] =
    lock.withPermit(f(KeyValueStoreEquivocationsTracker))
}

object BlockDagKeyValueStorage {
  implicit private val BlockDagKeyValueStorage_FromFileMetricsSource: Source =
    Metrics.Source(BlockStorageMetricsSource, "dag-key-value-store")

  val computedCoveringsMap = mutable.TreeMap.empty[Validator, Set[BlockMetadata]]

  private final case class DagStores[F[_]](
      metadata: BlockMetadataStore[F],
      metadataDb: KeyValueTypedStore[F, BlockHash, BlockMetadata],
      equivocations: EquivocationTrackerStore[F],
      equivocationsDb: KeyValueTypedStore[F, (Validator, SequenceNumber), Set[BlockHash]],
      latestMessages: KeyValueTypedStore[F, Validator, BlockHash],
      invalidBlocks: KeyValueTypedStore[F, BlockHash, BlockMetadata],
      deploys: KeyValueTypedStore[F, DeployId, BlockHash],
      accepted: KeyValueTypedStore[F, DeployChain, Unit],
      rejected: KeyValueTypedStore[F, DeployChain, Unit],
      latestFringes: KeyValueTypedStore[F, Long, DagFringe],
      finalityViews: KeyValueTypedStore[F, Validator, Long]
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
      rejectedIndexDb <- KeyValueStoreManager[F].database[DeployChain, Unit](
                          "rejected-index",
                          codecDeployChain,
                          scodec.codecs.ignore(0)
                        )
      acceptedIndexDb <- KeyValueStoreManager[F].database[DeployChain, Unit](
                          "accepted-index",
                          codecDeployChain,
                          scodec.codecs.ignore(0)
                        )
      latestFringesDB <- KeyValueStoreManager[F].database[Long, DagFringe](
                          "finalized-fringes",
                          scodec.codecs.vlong,
                          codecDagFringe
                        )
      finalityViewsDb <- KeyValueStoreManager[F].database[Validator, Long](
                          "finality-views",
                          codecValidator,
                          scodec.codecs.vlong
                        )
      blockMetadataStore <- BlockMetadataStore[F](blockMetadataDb, latestFringesDB, finalityViewsDb)
    } yield DagStores(
      blockMetadataStore,
      blockMetadataDb,
      equivocationTrackerIndex,
      equivocationTrackerDb,
      latestMessagesDb,
      invalidBlocksDb,
      deployIndexDb,
      acceptedIndexDb,
      rejectedIndexDb,
      latestFringesDB,
      finalityViewsDb
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
      stores.equivocations,
      stores.accepted,
      stores.rejected,
      stores.latestFringes,
      stores.finalityViews
    )
}
