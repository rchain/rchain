package coop.rchain.blockstorage.dag

import cats.Show
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
import coop.rchain.blockstorage.util.BlockMessageUtil._
import coop.rchain.casper.{v2, PrettyPrinter}
import coop.rchain.casper.protocol.{BlockMessage, DeployChain, StateMetadata}
import coop.rchain.casper.v2.core.Casper
import coop.rchain.casper.v2.core.Casper.FinalizationFringe
import coop.rchain.casper.v2.core.Validation.Slashing
import coop.rchain.casper.v2.stcasper.ConflictsResolver.ConflictResolution
import coop.rchain.casper.v2.stcasper.Validation.DummyOffence
import coop.rchain.crypto.codec.Base16
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, MetricsSemaphore}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.EquivocationRecord.SequenceNumber
import coop.rchain.models.Validator.Validator
import coop.rchain.models.block.StateHash.StateHash
import coop.rchain.models.{BlockHash, BlockMetadata, EquivocationRecord, Validator}
import coop.rchain.shared.syntax._
import coop.rchain.models.syntax._
import coop.rchain.shared.{Log, LogSource}
import coop.rchain.store.{KeyValueStoreManager, KeyValueTypedStore}

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
    latestFringesStore: KeyValueTypedStore[F, Long, DagFringe]
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
      st.childrenMap.get(blockHash).pure[F]

    def genesis: F[BlockHash] =
      st.heightMap.head._2.head.pure[F]

    def latestMessageHash(validator: Validator): F[Option[BlockHash]] =
      st.latestMessagesMap.get(validator).pure[F]

    def latestMessageHashes: F[Map[Validator, BlockHash]] = st.latestMessagesMap.pure[F]

    def invalidBlocks: F[Set[BlockMetadata]] =
      st.invalidBlocksSet.map(_.message).toList.traverse(this.lookupUnsafe).map(_.toSet)

    // latestBlockNumber, topoSort and lookupByDeployId are only used in BlockAPI.
    // Do they need to be part of the DAG current state or they can be moved to DAG storage directly?

    private def getMaxHeight = if (st.heightMap.nonEmpty) st.heightMap.last._1 + 1L else 0L

    def latestBlockNumber: F[Long] = getMaxHeight.pure[F]

    def isFinalized(blockHash: BlockHash): F[Boolean] = false.pure[F]

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

    override def finalizationState: BlockDagFinalizationState = st.finalizationState

    override def getPureState: BlockDagRepresentationState = st
    override def finalizationFringes: List[DagFringe]      = st.latestFringes
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
      heightMap      <- blockMetadataIndex.heightMap
      invalidBlocks <- invalidBlocksIndex.toMap
                        .map(_.toSeq.map(_._2).toSet)
                        .map(_.map(meta => Slashing(meta.blockHash, DummyOffence)))
      acceptedSet <- acceptedIndex.toMap.map(_.keySet)
      rejectedSet <- rejectedIndex.toMap.map(_.keySet)
      latestFringes <- latestFringesStore.toMap.map(
                        _.toList.sortBy(_._1).reverse.map(_._2)
                      )
    } yield KeyValueDagRepresentation(
      BlockDagRepresentationState(
        dagSet,
        latestMessages,
        childMap,
        heightMap,
        invalidBlocks,
        BlockDagFinalizationState(acceptedSet, rejectedSet),
        latestFringes
      )
    )

  def getRepresentation: F[BlockDagRepresentation[F]] =
    lock.withPermit(OptionT.fromOption(latestRepresentation.get(())).getOrElseF(representation))

  import coop.rchain.models.syntax._
  def insert(
      block: BlockMessage,
      invalid: Boolean,
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
    def doInsert: F[Unit] = {
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

        initMeta = BlockMetadata.fromBlock(block, invalid, List())
        js       = initMeta.justifications.map(_.latestBlockHash)
        jsLvl2 <- js
                   .traverse(blockMetadataIndex.getUnsafe)
                   .map(_.flatMap(_.justifications.map(_.latestBlockHash)))
        blockMetadata = initMeta.copy(parents = js diff jsLvl2)

        // Add block metadata
        _ <- blockMetadataIndex.add(blockMetadata)

        // Add deploys to deploy index storage
        deployHashes = deployData(block).map(_.sig).toList
        _            <- deployIndex.put(deployHashes.map(_ -> block.blockHash))

        // Update invalid index
        _ <- invalidBlocksIndex
              .put(blockMetadata.blockHash, blockMetadata)
              .whenA(invalid)

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

    def updateFinalization(
        newDag: BlockDagRepresentation[F]
    ) = {
      val casperMaxDepth = 100L
      val metaDag        = BlockMetadataDag(newDag)
      val safetyOracle   = BlockMetadataSafetyOracle()
      val casper         = BlockMetadataCasper(faultToleranceThreshold = -1, maxDepth = casperMaxDepth)

      implicit val show = new Show[BlockMetadata] {
        def show(meta: BlockMetadata): String =
          Base16.encode(meta.blockHash.toByteArray) + s" #${meta.seqNum}"
      }

      import v2.core.syntax.all._
      for {
        _   <- Log[F].info(s"Updating finalization state.")
        lms <- newDag.latestMessages.map(_.values.map(v => (v.sender, v)))
        _ <- latestFringesStore.toMap.map(_.toList.sortBy(_._1).reverse.headOption).flatMap {
              // fringe exists - try update
              case Some((latestIdx, DagFringe(latestFringe, lfs))) =>
                for {
                  _ <- Log[F].info(s"latestFringe idx $latestIdx")
                  latestFringeMeta <- latestFringe.toList
                                       .traverse {
                                         case (s, hs) =>
                                           hs.toList
                                             .traverse(h => newDag.lookupUnsafe(h))
                                             .map(v => (s, v.toSet))
                                       }
                  newFringesRecorded <- casper.finalise(
                                         lms.toList,
                                         metaDag,
                                         safetyOracle,
                                         latestFringeMeta
                                       )
                  _ <- mergeFOpt.traverse { merge =>
                        (latestFringeMeta +: newFringesRecorded)
                          .zip(newFringesRecorded)
                          .foldLeftM[F, (StateHash, Long)]((lfs, latestIdx + 1)) {
                            case ((prevLfs, nextIdx), (curFringe, nextFringe)) =>
                              val curFringeBlocks = curFringe.flatMap(_._2).map(_.blockHash)
                              val conflictSet = nextFringe
                                .filterNot {
                                  case (_, meta) =>
                                    (curFringeBlocks.toSet intersect meta.map(_.blockHash)).nonEmpty
                                }
                                .flatMap(_._2)
                                .toSet
                              for {
                                v <- conflictSet.toList.traverse { m =>
                                      for {
                                        js <- Sync[F].delay(m.justifications.map(_.latestBlockHash))
                                        finSet <- curFringe
                                                   .map(_._2)
                                                   .traverse { m =>
                                                     val stream = fs2.Stream
                                                       .emits(m.toList)
                                                       .covary[F] ++ metaDag
                                                       .selfJustificationChain(m.head)
                                                     stream
                                                       .takeWhile(m => !js.contains(m.blockHash))
                                                       .compile
                                                       .toList
                                                   }
                                                   .map(_.flatten)
                                      } yield (m, finSet.toSet)
                                    }
                                r <- merge(prevLfs, v.toSet)
                                _ <- Log[F].info(
                                      s"FF advanced to ${nextFringe.flatMap(_._2.map(_.seqNum)).mkString(";")}"
                                    )
                                (cr, sh) = r
                                _ <- rejectedIndex
                                      .putIfAbsent(cr.rejectedSet.map((_, ())).toList) >>
                                      acceptedIndex.putIfAbsent(cr.acceptedSet.map((_, ())).toList)
                                // record new fringes
                                _ <- latestFringesStore.put(
                                      nextIdx,
                                      DagFringe(
                                        nextFringe.map { case (s, m) => (s, m.map(_.blockHash)) },
                                        sh
                                      )
                                    )
                              } yield (sh, nextIdx + 1)
                          }
                      }
                } yield ()
              // does not exist - record block as a fringe
              case None => {
                val f = DagFringe(
                  block.body.state.bonds
                    .map(_.validator)
                    .map(v => (v, Set(block.blockHash))),
                  block.body.state.postStateHash
                )
                Log[F].info(s"latestFringesStore is empty. Inserting block as a fringe.") >>
                  latestFringesStore.put(0L, f)
              }
            }
      } yield ()
    }

    lock.withPermit(
      blockMetadataIndex
        .contains(block.blockHash)
        .ifM(logAlreadyStored, doInsert >> representation >>= updateFinalization) >>
        representation.map { r =>
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
      latestFringes: KeyValueTypedStore[F, Long, DagFringe]
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
      latestFringesDB
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
      stores.latestFringes
    )
}
