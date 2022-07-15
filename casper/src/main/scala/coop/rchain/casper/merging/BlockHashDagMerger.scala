package coop.rchain.casper.merging

import cats.effect.Concurrent
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.Message
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.syntax._
import coop.rchain.rholang.interpreter.RhoRuntime.RhoHistoryRepository
import coop.rchain.rholang.interpreter.merging.RholangMergingLogic
import coop.rchain.rspace.HotStoreTrieAction
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.merger.{ChannelChange, StateChange, StateChangeMerger}
import coop.rchain.rspace.merger.EventLogMergingLogic.NumberChannelsDiff
import coop.rchain.sdk.dag.merging.DagMergingLogic
import coop.rchain.sdk.dag.merging.DagMergingLogic._
import coop.rchain.shared.{Log, Stopwatch}
import coop.rchain.shared.syntax._
import scodec.bits.ByteVector
import cats.syntax.all._
import coop.rchain.rholang.syntax._
import coop.rchain.rspace.syntax._

object BlockHashDagMerger {
  def merge[F[_]: Concurrent: Log](
      tips: Set[BlockHash],
      finalFringe: Set[BlockHash],
      finalState: Blake2b256Hash,
      merger: BlockHashDagMerger,
      fringeStates: Map[Set[BlockHash], (Blake2b256Hash, Set[ByteString])],
      historyRepository: RhoHistoryRepository[F],
      blockIndex: BlockHash => F[BlockIndex],
      rejectionCost: DeployChainIndex => Long = DeployChainIndex.deployChainCost
  ): F[(Blake2b256Hash, Set[ByteString])] = {
    val (conflictScope, finalScope) = merger.computeMergingScope(tips, finalFringe)
    val loadIndices                 = List(conflictScope, finalScope).traverse(_.toList.traverse(blockIndex))
    loadIndices
      .flatMap {
        case List(conflictScopeIndices, finalScopeIndices) =>
          val allMergeableChannels = finalScopeIndices
            .flatMap(
              _.deployChains.map(_.eventLogIndex).flatMap(_.numberChannelsData.keySet)
            )
            .toSet
          historyRepository
            .readMergeableValues(finalState, allMergeableChannels)
            .map((conflictScopeIndices, finalScopeIndices, _))
      }
      .map {
        case (conflictScopeIndices, finalScopeIndices, finalStateMergeableValues) =>
          val rejectedInHostFringe = fringeStates.flatMap { case (k, v) => k.map((_, v._2)) }
          val (acceptedFinally, rejectedFinally) = finalScopeIndices.iterator
            .flatMap(b => b.deployChains.map(b.blockHash -> _))
            .partition {
              case (hash, deploy) =>
                rejectedInHostFringe(hash).contains(deploy.deploysWithCost.map(_.id).head)
            }
          merger
            .resolveConflicts(
              tips,
              finalFringe,
              finalScopeIndices.toSet,
              conflictScopeIndices.toSet,
              finalStateMergeableValues,
              acceptedFinally.map(_._2).toSet,
              rejectedFinally.map(_._2).toSet,
              rejectionCost
            )
      }
      .flatMap {
        case (toMerge, rejected) =>
          computeMergedState(toMerge, finalState, historyRepository).map { newState =>
            (newState, rejected.flatMap(_.deploysWithCost.map(_.id)))
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

final case class BlockHashDagMerger(
    msgMap: Map[BlockHash, Message[BlockHash, Validator]]
) {

  def computeMergingScope(
      tips: Set[BlockHash],
      finalFringe: Set[BlockHash]
  ): (Set[BlockHash], Set[BlockHash]) = {
    val seen   = msgMap.getUnsafe(_: BlockHash).seen
    val fringe = msgMap.getUnsafe(_: BlockHash).fringe
    val height = msgMap.getUnsafe(_: BlockHash).height

    val cScope = conflictScope(tips, finalFringe, seen)
    // lowest fringe required to to be able to resolve conflicts
    val lFringe = lowestFringe(cScope.map(fringe), height)
    val fScope  = finalScope(finalFringe, lFringe, seen)
    (cScope, fScope)
  }

  def resolveConflicts(
      tips: Set[BlockHash],
      finalFringe: Set[BlockHash],
      finalScope: Set[BlockIndex],
      conflictScope: Set[BlockIndex],
      finalStateMergeableValues: Map[Blake2b256Hash, Long],
      acceptedFinally: Set[DeployChainIndex],
      rejectedFinally: Set[DeployChainIndex],
      rejectionCost: DeployChainIndex => Long
  ): (Set[DeployChainIndex], Set[DeployChainIndex]) = {
    val mergeScopeIndices = (conflictScope ++ finalScope).map { b =>
      b.blockHash -> b.deployChains.toSet
    }.toMap
    val conflictSet = conflictScope.flatMap(_.deployChains)
    val finalSet    = finalScope.flatMap(_.deployChains)
    val mergeableDiffs = (conflictSet ++ finalSet)
      .map(b => b -> b.eventLogIndex.numberChannelsData)
      .toMap
    // TODO conflictsMap and dependentsMap computations are expensive
    //  can be cached and updated on new block added | new fringe finalized
    val (conflictsMap, dependencyMap) = DagMergingLogic.computeRelationMapForMergeSet(
      conflictSet,
      finalSet,
      DeployChainIndex.deploysAreConflicting,
      DeployChainIndex.isDependency
    )
    DagMergingLogic.resolveDag[BlockHash, DeployChainIndex, Blake2b256Hash, Blake2b256Hash](
      // DAG
      latestMessages = tips,
      seen = msgMap.getUnsafe(_: BlockHash).seen,
      // finalization
      latestFringe = finalFringe,
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
      initMergeableValues = finalStateMergeableValues
    )
  }
}
