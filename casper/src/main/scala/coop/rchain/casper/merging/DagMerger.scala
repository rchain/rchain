package coop.rchain.casper.merging

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.blockstorage.syntax._
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.rholang.interpreter.RhoRuntime.RhoHistoryRepository
import coop.rchain.rspace.HotStoreTrieAction
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.merger.{MergingLogic, StateChange, StateChangeMerger}
import coop.rchain.rspace.syntax._
import coop.rchain.shared.Log

object DagMerger {

  def merge[F[_]: Concurrent: Log](
      dag: BlockDagRepresentation[F],
      lfb: BlockHash,
      lfbPostState: Blake2b256Hash,
      index: BlockHash => F[Vector[DeployChainIndex]],
      historyRepository: RhoHistoryRepository[F]
  ): F[(Blake2b256Hash, Seq[ByteString])] =
    for {
      // all not finalized blocks (conflict set)
      nonFinalisedBlocks <- dag.nonFinalizedBlocks
      // blocks that see last finalized state
      actualBlocks <- dag.descendants(lfb)
      // blocks that does not see last finalized state
      lateBlocks = nonFinalisedBlocks diff actualBlocks

      actualSet <- actualBlocks.toList.traverse(index).map(_.flatten.toSet)
      // TODO reject only late units conflicting with finalised body
      lateSet <- lateBlocks.toList.traverse(index).map(_.flatten.toSet)

      branchesAreConflicting = (as: Set[DeployChainIndex], bs: Set[DeployChainIndex]) =>
        MergingLogic.areConflicting(
          as.map(_.eventLogIndex).toList.combineAll,
          bs.map(_.eventLogIndex).toList.combineAll
        )

      computeTrieActions = (baseState: Blake2b256Hash, changes: StateChange) => {
        val baseReader             = historyRepository.getHistoryReader(baseState).readerBinary
        val joinsPointerForChannel = (channel: Blake2b256Hash) => channel.pure[F]
        StateChangeMerger.computeTrieActions(changes, baseReader, joinsPointerForChannel)
      }

      applyTrieActions = (baseState: Blake2b256Hash, actions: Seq[HotStoreTrieAction]) =>
        historyRepository.reset(baseState).flatMap(_.doCheckpoint(actions).map(_.root))

      r <- ConflictSetMerger.merge[F, DeployChainIndex](
            baseState = lfbPostState,
            actualSet = actualSet,
            lateSet = lateSet,
            depends =
              (target, source) => MergingLogic.depends(target.eventLogIndex, source.eventLogIndex),
            conflicts = branchesAreConflicting,
            cost = r => r.deploysWithCost.map(_.cost).sum,
            stateChanges = r => r.stateChanges.pure,
            computeTrieActions = computeTrieActions,
            applyTrieActions = applyTrieActions
          )

      (newState, rejected) = r
      rejectedDeploys      = rejected.flatMap(_.deploysWithCost.map(_.id))
    } yield (newState, rejectedDeploys.toList)
}
