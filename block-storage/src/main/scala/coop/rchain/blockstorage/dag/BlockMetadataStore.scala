package coop.rchain.blockstorage.dag

import cats.Monad
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.casper.PrettyPrinter
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import coop.rchain.store.KeyValueTypedStore

import scala.collection.immutable.SortedMap

object BlockMetadataStore {
  def apply[F[_]: Sync: Log](
      blockMetadataStore: KeyValueTypedStore[F, BlockHash, BlockMetadata]
  ): F[BlockMetadataStore[F]] =
    for {
      _ <- Log[F].info("Building in-memory blockMetadataStore.")
      // Iterate over block metadata store and collect info for in-memory cache
      blockInfoMap <- blockMetadataStore.collect {
                       case (hash, metaData) =>
                         (hash, blockMetadataToInfo(metaData()))
                     }
      _           <- Log[F].info("Reading data from blockMetadataStore done.")
      dagState    = recreateInMemoryState(blockInfoMap.toMap)
      _           <- Log[F].info("Successfully built in-memory blockMetadataStore.")
      dagStateRef <- Ref[F].of(dagState)
    } yield new BlockMetadataStore[F](blockMetadataStore, dagStateRef)

  final case class BlockMetadataStoreInconsistencyError(message: String) extends Exception(message)

  private final case class DagState(
      dagSet: Set[BlockHash],
      childMap: Map[BlockHash, Set[BlockHash]],
      heightMap: SortedMap[Long, Set[BlockHash]]
  )

  def blockMetadataToInfo(blockMeta: BlockMetadata): BlockInfo =
    BlockInfo(
      blockMeta.blockHash,
      blockMeta.justifications,
      blockMeta.blockNum,
      blockMeta.validationFailed
    )

  class BlockMetadataStore[F[_]: Monad](
      private val store: KeyValueTypedStore[F, BlockHash, BlockMetadata],
      private val dagState: Ref[F, DagState]
  ) {
    def add(block: BlockMetadata): F[Unit] =
      for {
        // Update DAG state with new block
        _ <- dagState.update { st =>
              val blockInfo   = blockMetadataToInfo(block)
              val newDagState = addBlockToDagState(blockInfo)(st)
              validateDagState(newDagState)
            }

        // Update persistent block metadata store
        _ <- store.put(block.blockHash, block)
      } yield ()

    def get(hash: BlockHash): F[Option[BlockMetadata]] = store.get1(hash)

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
    val newHeightMap = if (!block.validationFailed) {
      val currSet = state.heightMap.getOrElse(block.blockNum, Set())
      state.heightMap.updated(block.blockNum, currSet + block.hash)
    } else state.heightMap

    state.copy(
      dagSet = newDagSet,
      childMap = newChildMap,
      heightMap = newHeightMap
    )
  }

  private def validateDagState(state: DagState): DagState = {
    // Validate height map index (block numbers) are in sequence without holes
    val m          = state.heightMap
    val (min, max) = if (m.nonEmpty) (m.firstKey, m.lastKey + 1) else (0L, 0L)
    assert(max - min == m.size.toLong, "DAG store height map has numbers not in sequence.")
    state
  }

  // Used to project part of the block metadata for in-memory initialization
  final case class BlockInfo(
      hash: BlockHash,
      parents: Set[BlockHash],
      blockNum: Long,
      validationFailed: Boolean
  )

  private def recreateInMemoryState(
      blocksInfoMap: Map[BlockHash, BlockInfo]
  ): DagState = {
    val emptyState: DagState =
      DagState(
        dagSet = Set(),
        childMap = Map(),
        heightMap = SortedMap()
      )

    // Add blocks to DAG state
    val dagState = blocksInfoMap.foldLeft(emptyState) {
      case (state, (_, block)) => addBlockToDagState(block)(state)
    }

    validateDagState(dagState)
  }
}
