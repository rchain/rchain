package coop.rchain.blockstorage.dag

import cats.data.OptionT
import cats.{Monad, Show}
import cats.effect.{Concurrent, Sync}
import cats.effect.concurrent.{Deferred, Ref}
import cats.mtl.MonadState
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.BlockDagStorage.DagFringe
import coop.rchain.casper.PrettyPrinter
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.{BlockHash, BlockMetadata}
import coop.rchain.models.Validator.Validator
import coop.rchain.models.block.StateHash.StateHash
import coop.rchain.shared.syntax._
import coop.rchain.shared.{AtomicMonadState, Log}
import coop.rchain.store.KeyValueTypedStore
import monix.execution.atomic.AtomicAny

import scala.collection.immutable.SortedMap
import scala.collection.mutable

object BlockMetadataStore {
  def apply[F[_]: Concurrent: Log](
      blockMetadataStore: KeyValueTypedStore[F, BlockHash, BlockMetadata],
      latestFringesStore: KeyValueTypedStore[F, Long, DagFringe],
      finalityViewsStore: KeyValueTypedStore[F, Validator, Long]
  ): F[BlockMetadataStore[F]] =
    for {
      _ <- Log[F].info("Building in-memory blockMetadataStore.")
      // Iterate over block metadata store and collect info for in-memory cache
      blockInfoMap <- blockMetadataStore.collect {
                       case (hash, metaData) =>
                         (hash, blockMetadataToInfo(metaData()))
                     }
      _             <- Log[F].info("Reading data from blockMetadataStore done.")
      dagState      = recreateInMemoryState(blockInfoMap.toMap)
      _             <- Log[F].info("Successfully built in-memory blockMetadataStore.")
      fringes       <- latestFringesStore.toMap
      ds1           = dagState.copy(fringesMap = SortedMap[Long, DagFringe]() ++ fringes.toList)
      finalityViews <- finalityViewsStore.toMap
      ds            = ds1.copy(finalityViewMap = finalityViews)
      cache         <- Ref.of[F, Map[BlockHash, Deferred[F, Option[BlockMetadata]]]](Map())
    } yield new BlockMetadataStore[F](
      blockMetadataStore,
      cache,
      new AtomicMonadState(AtomicAny(ds))
    )

  final case class BlockMetadataStoreInconsistencyError(message: String) extends Exception(message)

  private final case class DagState(
      dagSet: Set[BlockHash],
      childMap: Map[BlockHash, Map[Validator, Vector[BlockHash]]],
      witnessMap: Map[BlockHash, Map[Validator, BlockHash]],
      jsMap: Map[BlockHash, Set[BlockHash]],
      ValidatorsMap: Map[BlockHash, Validator],
      heightMap: SortedMap[Long, Set[BlockHash]],
      finalizedBlockSet: Set[BlockHash],
      fringesMap: SortedMap[Long, DagFringe],
      finalityViewMap: Map[Validator, Long]
  )

  def blockMetadataToInfo(blockMeta: BlockMetadata): BlockInfo =
    BlockInfo(
      blockMeta.blockHash,
      blockMeta.sender,
      blockMeta.justifications.map(_.latestBlockHash).toSet,
      blockMeta.blockNum,
      blockMeta.invalid
    )

  class BlockMetadataStore[F[_]: Concurrent](
      private val store: KeyValueTypedStore[F, BlockHash, BlockMetadata],
      private val cache: Ref[F, Map[BlockHash, Deferred[F, Option[BlockMetadata]]]],
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

    def addFringe(fringe: DagFringe): F[Unit] = dagState.modify { st =>
//      val fringeNum = fringe.num
//      assert(
//        st.fringesMap.get(fringeNum).forall(_ == fringe),
//        "Diverging finalization for different senders is detected."
//      )
      st.copy(
        fringesMap = st.fringesMap.updated(fringe.num, fringe)
        //finalityViewMap = st.finalityViewMap.updated(sender, fringe.num)
      )
    }

    import coop.rchain.shared.Caching._
    def get(hash: BlockHash): F[Option[BlockMetadata]] =
      memoize[F, BlockHash, BlockMetadata](store.get(_), cache)(hash)

    def getUnsafe(hash: BlockHash)(
        //implicit f: Sync[F],
        implicit line: sourcecode.Line,
        file: sourcecode.File,
        enclosing: sourcecode.Enclosing
    ): F[BlockMetadata] = {
      def source = s"${file.value}:${line.value} ${enclosing.value}"
      def errBlockHash =
        s"BlockMetadataStore is missing key ${PrettyPrinter.buildString(hash)}\n  $source"
      get(hash) >>= (_.liftTo(BlockMetadataStoreInconsistencyError(errBlockHash)))
    }

    // DAG state operations

    def dagSet: F[Set[BlockHash]] = dagState.get.map(_.dagSet)

    def fringesMap: F[SortedMap[Long, DagFringe]] = dagState.get.map(_.fringesMap)

    def finalityMap: F[Map[Validator, Long]] = dagState.get.map(_.finalityViewMap)

    def contains(hash: BlockHash): F[Boolean] = dagState.get.map(_.dagSet.contains(hash))

    def childMapData: F[Map[BlockHash, Map[Validator, Vector[BlockHash]]]] =
      dagState.get.map(_.childMap)

    def witnessMap: F[Map[BlockHash, Map[Validator, BlockHash]]] =
      dagState.get.map(_.witnessMap)

    def heightMap: F[SortedMap[Long, Set[BlockHash]]] =
      dagState.get.map(_.heightMap)
  }

  private def addBlockToDagState(block: BlockInfo)(state: DagState): DagState = {
    // Update dag set / all blocks in the DAG
    val newDagSet = state.dagSet + block.hash

    // Update children relation map
    val newChildren = block.justifications.map((_, Map(block.Validator -> block.hash))) +
      ((block.hash, Map()))
    val newChildMap = newChildren.foldLeft(state.childMap) {
      case (acc, (key, newChildMap)) =>
        val curChildren = acc.getOrElse(key, Map.empty[Validator, Vector[BlockHash]])
        val newChildren = newChildMap.headOption
          .map {
            case (s, child) =>
              curChildren.updated(s, curChildren.getOrElse(s, Vector()) :+ child)
          }
          .getOrElse(curChildren)
        acc.updated(key, newChildren)
    }

    // Update block height map
    val newHeightMap = if (!block.isInvalid) {
      val currSet = state.heightMap.getOrElse(block.blockNum, Set())
      state.heightMap.updated(block.blockNum, currSet + block.hash)
    } else state.heightMap

    val witnessingValidator = block.Validator
    def addWit(
        witMap: Map[BlockHash, Map[Validator, BlockHash]],
        js: BlockHash
    ): Map[BlockHash, Map[Validator, BlockHash]] = {
      val curVal = witMap.getOrElse(js, Map())
      curVal
        .get(witnessingValidator) // if there is already witness for Validator, stop recursion, return
        .map(_ => witMap)
        .getOrElse { // otherwise record witness and proceed with self child
          val recorded = witMap.updated(js, curVal + (witnessingValidator -> block.hash))
          val selfJsOpt =
            state.jsMap(js).find(j => state.ValidatorsMap(j) == state.ValidatorsMap(js))
          selfJsOpt.map { addWit(recorded, _) }.getOrElse(recorded)
        } // otherwise record
    }
    val newWitnessMap =
      block.justifications.foldLeft(
        state.witnessMap.updated(block.hash, Map.empty[Validator, BlockHash])
      )(addWit)

//
//    def addWit(
//        acc1: Map[BlockHash, Map[Validator, BlockHash]],
//        m: BlockHash
//    ): Map[BlockHash, Map[Validator, BlockHash]] = {
//      val newVal = acc1.updated(m, acc1.getOrElse(m, Map()) + (witnessingValidator -> block.hash))
//      val selfJsOpt =
//        state.jsMap.getOrElse(m, Set()).find(j => state.ValidatorsMap(j) == state.ValidatorsMap(m))
//      selfJsOpt
//        .map { selfJs =>
//          if (state.witnessMap.getOrElse(selfJs, Map()).contains(witnessingValidator))
//            newVal
//          else
//            addWit(acc1, selfJs)
//        }
//        .getOrElse(newVal)
//    }
//    val newWitnessMap = block.justifications.foldLeft(
//      state.witnessMap.updated(block.hash, Map.empty[Validator, BlockHash])
//    ) {
//      case (acc, js) =>
//        if (state.witnessMap.getOrElse(js, Map()).contains(witnessingValidator))
//          acc
//        else
//          addWit(acc, js)
//    }

    state.copy(
      dagSet = newDagSet,
      childMap = newChildMap,
      witnessMap = newWitnessMap,
      heightMap = newHeightMap,
      ValidatorsMap = state.ValidatorsMap + (block.hash -> block.Validator),
      jsMap = state.jsMap + (block.hash                 -> block.justifications)
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
      Validator: Validator,
      justifications: Set[BlockHash],
      blockNum: Long,
      isInvalid: Boolean
  )

  private def recreateInMemoryState(
      blocksInfoMap: Map[BlockHash, BlockInfo]
  ): DagState = {
    val emptyState: DagState =
      DagState(
        dagSet = Set(),
        childMap = Map(),
        witnessMap = Map(),
        jsMap = blocksInfoMap.mapValues(_.justifications),
        heightMap = SortedMap(),
        ValidatorsMap = blocksInfoMap.mapValues(_.Validator),
        finalizedBlockSet = Set(),
        fringesMap = SortedMap(),
        finalityViewMap = Map()
      )

    // Add blocks to DAG state
    val dagState = blocksInfoMap.toList.sortBy(_._2.blockNum).foldLeft(emptyState) {
      case (state, (_, block)) => addBlockToDagState(block)(state)
    }

    validateDagState(dagState)
  }
}
