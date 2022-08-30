package coop.rchain.casper.merging

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.{DagRepresentation, Message}
import coop.rchain.blockstorage.syntax._
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.FringeData
import coop.rchain.models.Validator.Validator
import coop.rchain.rholang.interpreter.RhoRuntime.RhoHistoryRepository
import coop.rchain.rholang.interpreter.merging.RholangMergingLogic
import coop.rchain.rholang.syntax._
import coop.rchain.rspace.HotStoreTrieAction
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.merger.EventLogMergingLogic.NumberChannelsDiff
import coop.rchain.rspace.merger.{ChannelChange, StateChange, StateChangeMerger}
import coop.rchain.rspace.syntax._
import coop.rchain.sdk.dag.merging.ConflictResolutionLogic
import coop.rchain.shared.{Log, Stopwatch}
import scodec.bits.ByteVector

/**
  * The scope to merge.
  * @param finalScope     blocks that cannot be altered
  * @param conflictScope  blocks that can be altered
  */
final case class MergeScope(finalScope: Set[BlockHash], conflictScope: Set[BlockHash])

object MergeScope {

  /**
    * Create Merge scope from DAG
    * @param mergeFringe tip messages of the DAG
    * @param finalFringe finalization fringe
    * @param childMap    children for a message
    * @param dagData     structure of the DAG
    * @return Merge scope and (optionally) block which should be used as a base for merge.
    *         By default merge should be done into the final state.
    */
  def fromDag(
      mergeFringe: Set[BlockHash],
      finalFringe: Set[BlockHash],
      childMap: Map[BlockHash, Set[BlockHash]],
      dagData: Map[BlockHash, Message[BlockHash, Validator]]
  ): (MergeScope, Option[BlockHash]) = {
    val pruneFringe = dagData.pruneFringe(finalFringe, childMap).map(_.id)
    fromFringes(mergeFringe, finalFringe, pruneFringe, dagData)
  }

  def fromFringes(
      mergeFringe: Set[BlockHash],
      finalFringe: Set[BlockHash],
      pruneFringe: Set[BlockHash],
      dagData: Map[BlockHash, Message[BlockHash, Validator]]
  ): (MergeScope, Option[BlockHash]) = {
    val mergeFringeMsgs = mergeFringe.map(dagData)
    val finalFringeMsgs = finalFringe.map(dagData)
    val pruneFringeMsgs = pruneFringe.map(dagData)

    // Conflict scope
    val cScope = dagData.between(mergeFringeMsgs, finalFringeMsgs)

    // Final scope
    val fScope = dagData.between(finalFringeMsgs, pruneFringeMsgs)

    // Scope ids
    val cScopeIds = cScope.map(_.id)
    val fScopeIds = fScope.map(_.id)

    // Find base message if final scope is empty (genesis merging scope)
    val baseMsg = Option(fScope).filter(_.isEmpty).flatMap { _ =>
      val genesisOpt = dagData.findWithEmptyParents
      assert(genesisOpt.nonEmpty, "Final scope is empty but no genesis found.")
      genesisOpt.map(_.id)
    }

    (MergeScope(fScopeIds, cScopeIds -- baseMsg.toSet), baseMsg)
  }

  def merge[F[_]: Concurrent: Log](
      mergeScope: MergeScope,
      baseState: Blake2b256Hash,
      fringeStates: Map[Set[BlockHash], FringeData],
      historyRepository: RhoHistoryRepository[F],
      blockIndex: BlockHash => F[BlockIndex],
      rejectionCost: DeployChainIndex => Long = DeployChainIndex.deployChainCost
  ): F[(Blake2b256Hash, Set[ByteString])] = {
    // if some indices can be computed - merge is impossible.
    val loadIndices = List(mergeScope.conflictScope, mergeScope.finalScope)
      .traverse(_.toList.traverse(blockIndex).map(_.toSet))

    loadIndices.flatMap {
      case List(conflictScope, finalScope) =>
        val conflictSet = conflictScope.flatMap(_.deployChains)
        val finalSet    = finalScope.flatMap(_.deployChains)
        // finalization decisions made in final set
        val (rejectedFinally, acceptedFinally) = {
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
