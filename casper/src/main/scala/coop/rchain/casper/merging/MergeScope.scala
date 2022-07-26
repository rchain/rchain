package coop.rchain.casper.merging

import cats.Applicative
import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.{Finalizer, Message}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.FringeData
import coop.rchain.models.Validator.Validator
import coop.rchain.models.syntax._
import coop.rchain.rholang.interpreter.RhoRuntime.RhoHistoryRepository
import coop.rchain.rholang.interpreter.merging.RholangMergingLogic
import coop.rchain.rholang.syntax._
import coop.rchain.rspace.HotStoreTrieAction
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.merger.EventLogMergingLogic.NumberChannelsDiff
import coop.rchain.rspace.merger.{ChannelChange, StateChange, StateChangeMerger}
import coop.rchain.rspace.syntax._
import coop.rchain.sdk.dag.merging.ConflictResolutionLogic
import coop.rchain.sdk.dag.merging.ConflictResolutionLogic.{conflictScope, finalScope}
import coop.rchain.sdk.syntax.all._
import coop.rchain.shared.{Log, Stopwatch}
import scodec.bits.ByteVector

/**
  * The scope to merge.
  * @param finalScope     blocks that cannot be altered
  * @param conflictScope  blocks that can be altered
  * @param baseState      base state to merge into
  */
final case class MergeScope(
    finalScope: Set[BlockHash],
    conflictScope: Set[BlockHash],
    baseState: Blake2b256Hash
)

object MergeScope {

  private def findGenesis(
      dagData: Map[BlockHash, Message[BlockHash, Validator]]
  ): Option[BlockHash] = {
    val candidates = dagData.filter {
      case (_, m) =>
        val edge      = m.seen == Set(m.id)
        val seenByAll = dagData.valuesIterator.forall(_.seen.contains(m.id))
        edge && seenByAll
    }
    if (candidates.size > 1) none[BlockHash] else candidates.headOption.map(_._1)
  }

  /**
    * Use dag data to optimise the merge scope.
    * For now covers only the case of genesis ceremony, when final set is empty so
    * genesis block can be removed from conflict set and merge can be done into genesis post state.
    */
  def optimise[F[_]: Applicative](
      mergeScope: MergeScope,
      dagData: Map[BlockHash, Message[BlockHash, Validator]],
      postState: BlockHash => F[Blake2b256Hash]
  ): F[MergeScope] = {
    val MergeScope(fScope, cScope, _) = mergeScope
    val genesisCase = {
      val gOpt   = findGenesis(dagData)
      val errStr = "Cannot create merge scope. Genesis case but genesis block cannot be found"
      assert(gOpt.isDefined, errStr)
      val genesis = gOpt.get
      postState(genesis).map(MergeScope(fScope, cScope - genesis, _))
    }
    // if final scope is empty - do special case for genesis
    if (fScope.isEmpty) genesisCase
    else mergeScope.pure
  }

  def apply(
      mergeFringe: Set[BlockHash],
      finalFringe: Set[BlockHash],
      finalState: Blake2b256Hash,
      dagData: Map[BlockHash, Message[BlockHash, Validator]]
  ): MergeScope = {
    val seen      = dagData.getUnsafe(_: BlockHash).seen
    val cScope    = conflictScope(mergeFringe, finalFringe, seen)
    val finalizer = Finalizer(dagData)
    val lFringe   = finalizer.lowestFringe(cScope.map(dagData)).map(_.id)
    val fScope    = finalScope(finalFringe, lFringe, seen)
    MergeScope(fScope, cScope, finalState)
  }

  def merge[F[_]: Concurrent: Log](
      mergeScope: MergeScope,
      fringeStates: Map[Set[BlockHash], FringeData],
      historyRepository: RhoHistoryRepository[F],
      blockIndex: BlockHash => F[BlockIndex],
      rejectionCost: DeployChainIndex => Long = DeployChainIndex.deployChainCost
  ): F[(Blake2b256Hash, Set[ByteString])] = {
    val MergeScope(finalScope, conflictScope, baseState) = mergeScope

    // if some indices can be computed - merge is impossible.
    val loadIndices =
      List(conflictScope, finalScope).traverse(_.toList.traverse(blockIndex).map(_.toSet))

    loadIndices.flatMap {
      case List(conflictScope, finalScope) =>
        val conflictSet = conflictScope.flatMap(_.deployChains)
        val finalSet    = finalScope.flatMap(_.deployChains)
        // finalization decisions made in final set
        val (acceptedFinally, rejectedFinally) = {
          val rejectionsMap = fringeStates.flatMap { case (k, v) => k.map((_, v.rejectedDeploys)) }
          finalScope.iterator
            .flatMap(b => b.deployChains.map(b.blockHash -> _))
            .partition {
              case (hash, deploy) =>
                rejectionsMap.get(hash).exists(_.contains(deploy.deploysWithCost.map(_.id).head))
            }
        }
        // mergeable channels
        val mergeableDiffsMap = conflictSet.map(b => b -> b.eventLogIndex.numberChannelsData).toMap
        val loadInitMergeableValues = historyRepository.readMergeableValues(
          baseState,
          mergeableDiffsMap.valuesIterator.flatMap(_.keySet).toSet
        )
        // TODO conflictsMap and dependentsMap computations are expensive
        //  can be cached and updated on new block added | new fringe finalized
        val (conflictsMap, dependencyMap) = ConflictResolutionLogic.computeRelationMapForMergeSet(
          conflictSet,
          finalSet,
          DeployChainIndex.deploysAreConflicting,
          DeployChainIndex.depends
        )
        val resolveConflicts = loadInitMergeableValues.map { initMergeableValues =>
          ConflictResolutionLogic.resolveConflictSet[DeployChainIndex, Blake2b256Hash](
            conflictSet = conflictSet,
            acceptedFinally = acceptedFinally.map(_._2).toSet,
            rejectedFinally = rejectedFinally.map(_._2).toSet,
            cost = rejectionCost,
            // relations
            conflictsMap = conflictsMap,
            dependencyMap = dependencyMap,
            // support for mergeable
            mergeableDiffs = mergeableDiffsMap,
            initMergeableValues = initMergeableValues
          )
        }
        resolveConflicts.flatMap {
          case (toMerge, rejected) =>
            computeMergedState(toMerge, baseState, historyRepository).map { newState =>
              (newState, rejected.flatMap(_.deploysWithCost.map(_.id)))
            }
        }
    }
  }

  /** Merge set of indices into base state and produce new state. */
  def computeMergedState[F[_]: Concurrent: Log](
      toMerge: Set[DeployChainIndex],
      baseState: Blake2b256Hash,
      historyRepository: RhoHistoryRepository[F]
  ): F[Blake2b256Hash] =
    for {
      // Merge the state
      historyReader <- historyRepository.getHistoryReader(baseState)
      baseReader    = historyReader.readerBinary
      baseGetData   = historyReader.getData _
      overrideTrieAction = (
          hash: Blake2b256Hash,
          changes: ChannelChange[ByteVector],
          numberChs: NumberChannelsDiff
      ) =>
        numberChs.get(hash).traverse {
          RholangMergingLogic.calculateNumberChannelMerge(hash, _, changes, baseGetData)
        }
      computeTrieActions = (changes: StateChange, mergeableChs: NumberChannelsDiff) =>
        StateChangeMerger.computeTrieActions(
          changes,
          baseReader,
          mergeableChs,
          overrideTrieAction
        )
      applyTrieActions = (actions: Seq[HotStoreTrieAction]) =>
        historyRepository.reset(baseState).flatMap(_.doCheckpoint(actions).map(_.root))
      allChanges     = toMerge.map(_.stateChanges).toList.combineAll
      mergeableDiffs = toMerge.map(b => b -> b.eventLogIndex.numberChannelsData).toMap
      r <- Stopwatch.duration(
            computeTrieActions(allChanges, mergeableDiffs.values.toList.combineAll)
          )
      (trieActions, computeActionsTime) = r
      r                                 <- Stopwatch.duration(applyTrieActions(trieActions))
      (newState, applyActionsTime)      = r
      overallChanges                    = s"${allChanges.datumsChanges.size} D, ${allChanges.kontChanges.size} K, ${allChanges.consumeChannelsToJoinSerializedMap.size} J"
      logStr = s"Merging done. Changes: $overallChanges; " +
        s"trie actions (${trieActions.size}) computed in ${computeActionsTime}; " +
        s"actions applied in ${applyActionsTime}"
      _ <- Log[F].debug(logStr)
    } yield newState
}
