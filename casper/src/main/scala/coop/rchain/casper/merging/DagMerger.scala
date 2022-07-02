package coop.rchain.casper.merging

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.Message
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.syntax._
import coop.rchain.rholang.interpreter.RhoRuntime.RhoHistoryRepository
import coop.rchain.rholang.interpreter.merging.RholangMergingLogic
import coop.rchain.rholang.syntax._
import coop.rchain.rspace.HotStoreTrieAction
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.merger.{ChannelChange, StateChange, StateChangeMerger}
import coop.rchain.rspace.merger.EventLogMergingLogic.NumberChannelsDiff
import coop.rchain.sdk.dag.merging.DagMergingLogic._
import coop.rchain.sdk.dag.merging.data._
import coop.rchain.sdk.dag.merging.DagMergingLogic
import coop.rchain.shared.{Log, Stopwatch}
import scodec.bits.ByteVector
import coop.rchain.shared.syntax._
import coop.rchain.rspace.syntax._

object DagMerger {

  private def resolveConflicts[F[_]: Concurrent: Log](
      tips: Set[BlockHash],
      finalFringe: Set[BlockHash],
      finalState: Blake2b256Hash,
      msgMap: Map[BlockHash, Message[BlockHash, Validator]],
      acceptedFinally: Set[DeployChainIndex],
      rejectedFinally: Set[DeployChainIndex],
      historyRepository: RhoHistoryRepository[F],
      blockIndex: BlockHash => F[BlockIndex],
      rejectionCost: DeployChainIndex => Long
  ): F[Merge[Blake2b256Hash, DeployChainIndex]] = {
    val cScope = conflictScope(tips, finalFringe, msgMap.getUnsafe(_: BlockHash).seen)
    // lowest fringe required to to be able to resolve conflicts
    val lFringe = lowestFringe(
      cScope.map(m => msgMap.getUnsafe(m).fringe),
      msgMap.getUnsafe(_: BlockHash).height
    )
    val fScope     = finalScope(finalFringe, lFringe, msgMap.getUnsafe(_: BlockHash).seen)
    val mergeScope = cScope ++ fScope

    val prepareMergingIndicesF = mergeScope.toList
      .traverse(b => blockIndex(b).map(x => b -> x.deployChains.toSet))
      .map(_.toMap)

    for {
      mergeScopeIndices <- prepareMergingIndicesF
      conflictSet       = mergeScopeIndices.filterKeys(cScope).values.flatten.toSet
      finalSet          = mergeScopeIndices.filterKeys(fScope).values.flatten.toSet
      // diffs of mergeable values inside a conflict set
      mergeableDiffs = mergeScopeIndices
        .filterKeys(cScope)
        .values
        .flatten
        .map(b => b -> b.eventLogIndex.numberChannelsData)
        .toMap
      allMergeables       = mergeableDiffs.values.flatMap(_.keySet).toSet
      initMergeableValues <- historyRepository.readMergeableValues(finalState, allMergeables)
      // TODO conflictsMap and dependentsMap computations are expensive
      //  can be cached and updated on new block added | new fringe finalized
      (conflictsMap, dependencyMap) = DagMergingLogic.computeRelationMapForMergeSet(
        conflictSet,
        finalSet,
        DeployChainIndex.deploysAreConflicting,
        DeployChainIndex.isDependency
      )
    } yield DagMergingLogic.mergeDag[BlockHash, DeployChainIndex, Blake2b256Hash, Blake2b256Hash](
      // DAG
      latestMessages = tips,
      seen = msgMap.getUnsafe(_: BlockHash).seen,
      // finalization
      latestFringe = finalFringe,
      fringeState = finalState,
      acceptedFinally = acceptedFinally,
      rejectedFinally = rejectedFinally,
      // deploys
      deploysIndex = mergeScopeIndices,
      cost = rejectionCost,
      // relations
      conflictsMap = conflictsMap,
      dependencyMap = dependencyMap,
      // support for mergeable
      mergeableDiffs = mergeableDiffs,
      initMergeableValues = initMergeableValues
    )
  }

  /** Merge set of indices into base state and produce new state. */
  private def mergeState[F[_]: Concurrent: Log](
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
        s"trie actions (${trieActions.size}) compited in ${computeActionsTime}; " +
        s"actions applied in ${applyActionsTime}"
      _ <- Log[F].debug(logStr)
    } yield newState

  /**
    * General function to merge all kinds of DAGs
    * @param tips tips of the DAG with post state hashes
    * @param finalFringe finalization fringe
    * @param finalStateDef deferred for a final state (to not wait for state to be merged)
    * @param msgMap [[Message]] instance for a block
    * @param finalityState deploys that are rejected and accepted finally
    * @param historyRepository history repository instance
    * @param postState post state has for a block
    * @tparam F
    * @return state hash of merged
    */
  def merge[F[_]: Concurrent: Log](
      tips: Set[BlockHash],
      finalFringe: Set[BlockHash],
      finalState: Blake2b256Hash,
      msgMap: Map[BlockHash, Message[BlockHash, Validator]],
      acceptedFinally: Set[DeployChainIndex],
      rejectedFinally: Set[DeployChainIndex],
      historyRepository: RhoHistoryRepository[F],
      blockIndex: BlockHash => F[BlockIndex],
      rejectionCost: DeployChainIndex => Long = DeployChainIndex.deployChainCost
  ): F[(Blake2b256Hash, Set[ByteString])] =
    resolveConflicts(
      tips,
      finalFringe,
      finalState,
      msgMap,
      acceptedFinally,
      rejectedFinally,
      historyRepository,
      blockIndex,
      rejectionCost
    ).flatMap {
      case Merge(baseState, toMerge, rejected) =>
        mergeState(toMerge, baseState, historyRepository).map { newState =>
          (newState, rejected.flatMap(_.deploysWithCost.map(_.id)))
        }
    }
}
