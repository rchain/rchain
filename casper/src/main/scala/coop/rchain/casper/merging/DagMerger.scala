package coop.rchain.casper.merging

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.blockstorage.syntax._
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.rholang.interpreter.RhoRuntime.RhoHistoryRepository
import coop.rchain.rholang.interpreter.merging.RholangMergingLogic
import coop.rchain.rspace.HotStoreTrieAction
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.merger.MergingLogic.NumberChannelsDiff
import coop.rchain.rspace.merger.{ChannelChange, MergingLogic, StateChange, StateChangeMerger}
import coop.rchain.rspace.syntax._
import coop.rchain.shared.Log
import scodec.bits.ByteVector

object DagMerger {

  def costOptimalRejectionAlg: DeployChainIndex => Long =
    (r: DeployChainIndex) => r.deploysWithCost.map(_.cost).sum

  def merge[F[_]: Concurrent: Log](
      dag: BlockDagRepresentation[F],
      lfb: BlockHash,
      lfbPostState: Blake2b256Hash,
      index: BlockHash => F[Vector[DeployChainIndex]],
      historyRepository: RhoHistoryRepository[F],
      rejectionCostF: DeployChainIndex => Long
  ): F[(Blake2b256Hash, Seq[ByteString])] =
    for {
      // blocks that see last finalized state
      actualBlocks <- dag.descendants(lfb)
      // all not finalized blocks (conflict set)
      nonFinalisedBlocks = dag.nonFinalizedSet
      // blocks that does not see last finalized state
      lateBlocks = nonFinalisedBlocks diff actualBlocks

      actualSet <- actualBlocks.toList.traverse(index).map(_.flatten.toSet)
      // TODO reject only late units conflicting with finalised body
      lateSet <- lateBlocks.toList.traverse(index).map(_.flatten.toSet)

      branchesAreConflicting = (as: Set[DeployChainIndex], bs: Set[DeployChainIndex]) =>
        (as.flatMap(_.deploysWithCost.map(_.id)) intersect bs.flatMap(_.deploysWithCost.map(_.id))).nonEmpty ||
          MergingLogic.areConflicting(
            as.map(_.eventLogIndex).toList.combineAll,
            bs.map(_.eventLogIndex).toList.combineAll
          )

      // Base state reader
      baseReader       = historyRepository.getHistoryReader(lfbPostState)
      baseReaderBinary = baseReader.readerBinary
      baseGetData      = baseReader.getData _

      overrideTrieAction = (
          hash: Blake2b256Hash,
          changes: ChannelChange[ByteVector],
          numberChs: NumberChannelsDiff
      ) =>
        numberChs.get(hash).traverse {
          RholangMergingLogic.calculateNumberChannelMerge(hash, changes, baseGetData)
        }

      computeTrieActions = (changes: StateChange, mergeableChs: NumberChannelsDiff) => {
        StateChangeMerger.computeTrieActions(
          changes,
          baseReaderBinary,
          mergeableChs,
          overrideTrieAction
        )
      }

      applyTrieActions = (actions: Seq[HotStoreTrieAction]) =>
        historyRepository.reset(lfbPostState).flatMap(_.doCheckpoint(actions).map(_.root))

      r <- ConflictSetMerger.merge[F, DeployChainIndex](
            actualSet = actualSet,
            lateSet = lateSet,
            depends =
              (target, source) => MergingLogic.depends(target.eventLogIndex, source.eventLogIndex),
            conflicts = branchesAreConflicting,
            cost = rejectionCostF,
            stateChanges = _.stateChanges.pure,
            mergeableChannels = _.eventLogIndex.numberChannelsData,
            computeTrieActions = computeTrieActions,
            applyTrieActions = applyTrieActions
          )

      (newState, rejected) = r
      rejectedDeploys      = rejected.flatMap(_.deploysWithCost.map(_.id))
    } yield (newState, rejectedDeploys.toList)
}
