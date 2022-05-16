package coop.rchain.casper.merging

import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.dag.{BlockDagStorage, DagRepresentation}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.rholang.interpreter.RhoRuntime.RhoHistoryRepository
import coop.rchain.rholang.interpreter.merging.RholangMergingLogic
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
import coop.rchain.shared.Log
import scodec.bits.ByteVector

object DagMerger {

  def deployChainCost(r: DeployChainIndex): Long = r.deploysWithCost.map(_.cost).sum

  def merge[F[_]: Concurrent: BlockDagStorage: Log](
      dag: DagRepresentation,
      finalState: Blake2b256Hash,
      deployChainIndex: BlockHash => F[Vector[DeployChainIndex]],
      historyRepository: RhoHistoryRepository[F]
  ): F[(Blake2b256Hash, Seq[DeployId])] =
    for {
      // all not finalized blocks (conflict set)
      conflictSet <- (dag.dagSet -- dag.finalizedBlocksSet).toVector.flatTraverse(deployChainIndex)
      branchesAreConflicting = (as: Set[DeployChainIndex], bs: Set[DeployChainIndex]) =>
        (as.flatMap(_.deploysWithCost.map(_.id)) intersect bs.flatMap(_.deploysWithCost.map(_.id))).nonEmpty ||
          EventLogMergingLogic.areConflicting(
            as.map(_.eventLogIndex).toList.combineAll,
            bs.map(_.eventLogIndex).toList.combineAll
          )
      historyReader <- historyRepository.getHistoryReader(finalState)
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
        historyRepository.reset(finalState).flatMap(_.doCheckpoint(actions).map(_.root))

      r <- ConflictSetMerger.merge[F, DeployChainIndex](
            mergeSet = conflictSet.toSet,
            depends = (target, source) =>
              EventLogMergingLogic.depends(target.eventLogIndex, source.eventLogIndex),
            conflicts = branchesAreConflicting,
            cost = deployChainCost,
            stateChanges = _.stateChanges.pure,
            mergeableChannels = _.eventLogIndex.numberChannelsData,
            computeTrieActions = computeTrieActions,
            applyTrieActions = applyTrieActions,
            getData = historyReader.getData
          )

      (newState, rejected) = r
      rejectedDeploys      = rejected.flatMap(_.deploysWithCost.map(_.id))
    } yield (newState, rejectedDeploys.toList)
}
