package coop.rchain.casper.merging

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.dag.{DagOps, DagReader}
import coop.rchain.rholang.interpreter.RhoRuntime.RhoHistoryRepository
import coop.rchain.rspace.HotStoreTrieAction
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.merger.{MergingLogic, StateChange, StateChangeMerger}
import coop.rchain.rspace.syntax._

object DagMerger {

  /** Type for minimal rejection unit - set of dependent deploys executed inside one block */
  type R = DeployChainIndex

  def merge[F[_]: Concurrent, V](
      tips: List[V],
      lfb: V,
      dag: DagReader[F, V],
      isFinalised: V => F[Boolean],
      index: V => F[Vector[R]],
      postState: V => StateHash,
      historyRepository: RhoHistoryRepository[F]
  ): F[(StateHash, Seq[ByteString])] = {

    // all not finalized blocks (conflict set)
    val collectNotFinalisedBlocks = DagOps
      .bfTraverseF(tips)(
        v =>
          for {
            pOpt <- dag.parents(v).map(_.getOrElse(Set.empty))
            r    <- pOpt.toList.filterA(isFinalised(_).not)
          } yield r
      )
      .toSet

    // descendants of LFB
    val collectLfbDescendants = DagOps
      .bfTraverseF(List(lfb))(
        v => dag.children(v).map(_.getOrElse(Set.empty).toList)
      )
      .toSet
      .map(_ - lfb)

    for {
      nonFinalisedBlocks <- collectNotFinalisedBlocks
      actualBlocks       <- collectLfbDescendants
      lateBlocks         = nonFinalisedBlocks diff actualBlocks

      actualSet <- actualBlocks.toList.traverse(index).map(_.flatten)
      // TODO reject only late units conflicting with finalised body
      lateSet <- lateBlocks.toList.traverse(index).map(_.flatten)

      branchesAreConflicting = (as: Set[R], bs: Set[R]) =>
        MergingLogic.areConflicting(
          as.map(_.eventLogIndex).toList.combineAll,
          bs.map(_.eventLogIndex).toList.combineAll
        )

      computeTrieActions = (baseState: Blake2b256Hash, changes: StateChange) => {
        val baseReader = historyRepository.getHistoryReader(baseState).readerBinary
        val joinsPointerForChannel = (channel: Blake2b256Hash) =>
          historyRepository.getJoinMapping(Seq(channel)).map(_.head)
        StateChangeMerger.computeTrieActions(changes, baseReader, joinsPointerForChannel)
      }

      applyTrieActions = (baseState: Blake2b256Hash, actions: Seq[HotStoreTrieAction]) =>
        historyRepository.reset(baseState).flatMap(_.doCheckpoint(actions).map(_.root))

      // set that should be merged into LFB
      lfbPostState = Blake2b256Hash.fromByteString(postState(lfb))

      r <- ConflictSetMerger.merge[F, R](
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
    } yield (ByteString.copyFrom(newState.bytes.toArray), rejectedDeploys)
  }
}
