package coop.rchain.blockstorage.dag

import cats.Monad
import cats.effect.Sync
import cats.mtl.MonadState
import cats.syntax.all._
import coop.rchain.casper.PrettyPrinter
import coop.rchain.dag.DagOps
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.shared.syntax._
import coop.rchain.shared.{AtomicMonadState, Log}
import coop.rchain.store.KeyValueTypedStore
import monix.execution.atomic.AtomicAny

import scala.collection.immutable.SortedMap

object BlockMetadataStore {
  def apply[F[_]: Sync: Log](
      blockMetadataStore: KeyValueTypedStore[F, BlockHash, BlockMetadata],
      lastFinalizedBlock: Option[BlockHash]
  ): F[BlockMetadataStore[F]] =
    for {
      _ <- Log[F].info("Building in-memory blockMetadataStore.")
      // Iterate over block metadata store and collect info for in-memory cache
      blockInfoMap <- blockMetadataStore.collect {
                       case (hash, metaData) =>
                         (hash, blockMetadataToInfo(metaData()))
                     }
      _        <- Log[F].info("Reading data from blockMetadataStore done.")
      dagState = recreateInMemoryState(blockInfoMap.toMap, lastFinalizedBlock)
      _        <- Log[F].info("Successfully built in-memory blockMetadataStore.")
    } yield new BlockMetadataStore[F](
      blockMetadataStore,
      new AtomicMonadState(AtomicAny(dagState))
    )

  final case class BlockMetadataStoreInconsistencyError(message: String) extends Exception(message)

  private final case class DagState(
      dagSet: Set[BlockHash],
      childMap: Map[BlockHash, Set[BlockHash]],
      heightMap: SortedMap[Long, Set[BlockHash]],
      finalizedBlockSet: Set[BlockHash]
  )

  private def blockMetadataToInfo(blockMeta: BlockMetadata): BlockInfo =
    BlockInfo(blockMeta.blockHash, blockMeta.parents.toSet, blockMeta.blockNum, blockMeta.invalid)

  class BlockMetadataStore[F[_]: Monad](
      private val store: KeyValueTypedStore[F, BlockHash, BlockMetadata],
      private val dagState: MonadState[F, DagState]
  ) {
    def add(block: BlockMetadata): F[Unit] =
      for {
        // Update DAG state with new block
        _ <- dagState.modify { st =>
              val blockInfo   = blockMetadataToInfo(block)
              val newDagState = addBlockToDagState(blockInfo)(st)
              validateDagState(newDagState)
            }

        // Update persistent block metadata store
        _ <- store.put(block.blockHash, block)
      } yield ()

    def addFinalizedBlock(finalizedBlockHash: BlockHash): F[Unit] =
      dagState.modify(st => st.copy(finalizedBlockSet = st.finalizedBlockSet + finalizedBlockHash))

    def get(hash: BlockHash): F[Option[BlockMetadata]] = store.get(hash)

    def getUnsafe(hash: BlockHash)(
        implicit f: Sync[F],
        line: sourcecode.Line,
        file: sourcecode.File,
        enclosing: sourcecode.Enclosing
    ): F[BlockMetadata] = {
      def source = s"${file.value}:${line.value} ${enclosing.value}"
      def errMsg =
        s"BlockMetadataStore is missing key ${PrettyPrinter.buildString(hash)}\n  $source"
      get(hash) >>= (_.liftTo(BlockMetadataStoreInconsistencyError(errMsg)))
    }

    // DAG state operations

    def dagSet: F[Set[BlockHash]] = dagState.get.map(_.dagSet)

    def contains(hash: BlockHash): F[Boolean] = dagState.get.map(_.dagSet.contains(hash))

    def childMapData: F[Map[BlockHash, Set[BlockHash]]] =
      dagState.get.map(_.childMap)

    def heightMap: F[SortedMap[Long, Set[BlockHash]]] =
      dagState.get.map(_.heightMap)

    def finalizedBlockSet: F[Set[BlockHash]] = dagState.get.map(_.finalizedBlockSet)
  }

  private def addBlockToDagState(block: BlockInfo)(state: DagState): DagState = {
    // Update dag set / all blocks in the DAG
    val newDagSet = state.dagSet + block.hash

    // Update children relation map
    val blockChilds = block.parents.map((_, Set(block.hash))) + ((block.hash, Set()))
    val newChildMap = blockChilds.foldLeft(state.childMap) {
      case (acc, (key, newChildren)) =>
        val currChildren = acc.getOrElse(key, Set.empty[BlockHash])
        acc.updated(key, currChildren ++ newChildren)
    }

    // Update block height map
    val newHeightMap = if (!block.isInvalid) {
      val currSet = state.heightMap.getOrElse(block.blockNum, Set())
      state.heightMap.updated(block.blockNum, currSet + block.hash)
    } else state.heightMap

    state.copy(dagSet = newDagSet, childMap = newChildMap, heightMap = newHeightMap)
  }

  private def validateDagState(state: DagState): DagState = {
    // Validate height map index (block numbers) are in sequence without holes
    val m          = state.heightMap
    val (min, max) = if (m.nonEmpty) (m.firstKey, m.lastKey + 1) else (0L, 0L)
    assert(max - min == m.size.toLong, "DAG store height map has numbers not in sequence.")
    state
  }

  // Used to project part of the block metadata for in-memory initialization
  private final case class BlockInfo(
      hash: BlockHash,
      parents: Set[BlockHash],
      blockNum: Long,
      isInvalid: Boolean
  )

  private def recreateInMemoryState(
      blocksInfoMap: Map[BlockHash, BlockInfo],
      lastFinalizedBlockHash: Option[BlockHash]
  ): DagState = {
    val emptyState =
      DagState(dagSet = Set(), childMap = Map(), heightMap = SortedMap(), finalizedBlockSet = Set())

    // Add blocks to DAG state
    val dagState = blocksInfoMap.foldLeft(emptyState) {
      case (state, (_, block)) => addBlockToDagState(block)(state)
    }

    // Calculate set of finalized blocks
    val finalizedBlockSet = lastFinalizedBlockHash.fold(Set.empty[BlockHash])(lbh => {
      DagOps
        .bfTraverse(List(lbh))(
          bh => blocksInfoMap.get(bh).map(_.parents.toList).getOrElse(List.empty)
        )
        .toSet
    })

    // Update DAG state with finalized blocks
    val newDagState = dagState.copy(finalizedBlockSet = finalizedBlockSet)

    validateDagState(newDagState)
  }
}
