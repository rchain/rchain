package coop.rchain.casper.merging

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.{BlockDagStorage, DagRepresentation}
import coop.rchain.blockstorage.syntax._
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.rholang.interpreter.RhoRuntime.RhoHistoryRepository
import coop.rchain.rholang.interpreter.merging.RholangMergingLogic
import coop.rchain.rholang.interpreter.merging.RholangMergingLogic.convertToReadNumber
import coop.rchain.rspace.HotStoreTrieAction
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.merger.EventLogMergingLogic.NumberChannelsDiff
import coop.rchain.rspace.merger.{
  ChannelChange,
  EventLogMergingLogic,
  StateChange,
  StateChangeMerger
}
import coop.rchain.rspace.syntax._
import coop.rchain.shared.{Log, Stopwatch}
import scodec.bits.ByteVector

object DagMerger {

  def costOptimalRejectionAlg: DeployChainIndex => Long =
    (r: DeployChainIndex) => r.deploysWithCost.map(_.cost).sum

  def merge[F[_]: Concurrent: BlockDagStorage: Log](
      dag: DagRepresentation,
      fringe: Seq[BlockHash],
      lfbPostState: Blake2b256Hash,
      index: BlockHash => F[Vector[DeployChainIndex]],
      historyRepository: RhoHistoryRepository[F],
      rejectionCostF: DeployChainIndex => Long
  ): F[(Blake2b256Hash, Seq[ByteString])] =
    for {
      // all not finalized blocks (conflict set)
      nonFinalisedBlocks <- dag.nonFinalizedBlocks
      // blocks that see last finalized state
      // TODO: return all blocks as descendants if fringe is empty
      actualBlocks = if (fringe.nonEmpty) dag.descendants(fringe.head) else dag.dagSet
      // blocks that does not see last finalized state
      lateBlocks = nonFinalisedBlocks diff actualBlocks

      actualSet <- actualBlocks.toList.traverse(index).map(_.flatten.toSet)
      // TODO reject only late units conflicting with finalised body
      lateSet <- lateBlocks.toList.traverse(index).map(_.flatten.toSet)

      branchesAreConflicting = (as: Set[DeployChainIndex], bs: Set[DeployChainIndex]) =>
        (as.flatMap(_.deploysWithCost.map(_.id)) intersect bs.flatMap(_.deploysWithCost.map(_.id))).nonEmpty ||
          EventLogMergingLogic.areConflicting(
            as.map(_.eventLogIndex).toList.combineAll,
            bs.map(_.eventLogIndex).toList.combineAll
          )
      historyReader <- historyRepository.getHistoryReader(lfbPostState)
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
        historyRepository.reset(lfbPostState).flatMap(_.doCheckpoint(actions).map(_.root))

      baseMergeableChRes <- actualSet
                             .map(_.eventLogIndex.numberChannelsData)
                             .flatMap(_.keys)
                             .toList
                             .traverse(
                               channelHash =>
                                 convertToReadNumber(baseGetData)
                                   .apply(channelHash)
                                   .map(res => (channelHash, res.getOrElse(0L)))
                             )
                             .map(_.toMap)
      (toMerge, rejected, logStr1) = ConflictSetMerger
        .merge[DeployChainIndex](
          actualSet = actualSet,
          lateSet = lateSet,
          depends = (target, source) =>
            EventLogMergingLogic.depends(target.eventLogIndex, source.eventLogIndex),
          conflicts = branchesAreConflicting,
          cost = rejectionCostF,
          mergeableChannels = _.eventLogIndex.numberChannelsData,
          baseMergeableChRes
        )
      r = Stopwatch.profile(toMerge.toList.map(_.stateChanges).combineAll)

      (allChanges, combineAllChanges) = r

      // All number channels merged
      // TODO: Negative or overflow should be rejected before!
      allMergeableChannels = toMerge.toList
        .map(_.eventLogIndex.numberChannelsData)
        .combineAll

      r                                 <- Stopwatch.duration(computeTrieActions(allChanges, allMergeableChannels))
      (trieActions, computeActionsTime) = r
      r                                 <- Stopwatch.duration(applyTrieActions(trieActions))
      (newState, applyActionsTime)      = r
      overallChanges                    = s"${allChanges.datumsChanges.size} D, ${allChanges.kontChanges.size} K, ${allChanges.consumeChannelsToJoinSerializedMap.size} J"
      logStr = s"Merging done: " +
        s"late set size ${lateSet.size}; " +
        s"actual set size ${actualSet.size}; " +
        logStr1 +
        s"changes combined (${overallChanges}) in ${combineAllChanges}; " +
        s"trie actions (${trieActions.size}) in ${computeActionsTime}; " +
        s"actions applied in ${applyActionsTime}"
      _ <- Log[F].debug(logStr)

      rejectedDeploys = rejected.flatMap(_.deploysWithCost.map(_.id))
    } yield (newState, rejectedDeploys.toList)
}
