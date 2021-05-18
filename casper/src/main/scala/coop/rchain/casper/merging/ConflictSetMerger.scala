package coop.rchain.casper.merging

import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.rspace.HotStoreTrieAction
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.merger.StateChange._
import coop.rchain.rspace.merger._

object ConflictSetMerger {

  /** R is a type for minimal rejection unit */
  def merge[F[_]: Concurrent, R](
      baseState: Blake2b256Hash,
      actualSet: List[R],
      lateSet: List[R],
      depends: (R, R) => Boolean,
      conflicts: (Set[R], Set[R]) => Boolean,
      cost: R => Long,
      stateChanges: R => F[StateChange],
      computeTrieActions: (Blake2b256Hash, StateChange) => F[List[HotStoreTrieAction]],
      applyTrieActions: (Blake2b256Hash, Seq[HotStoreTrieAction]) => F[Blake2b256Hash]
  ): F[(Blake2b256Hash, List[R])] = {

    type Branch    = Set[R]
    type Rejection = Set[Branch]

    /** compute optimal rejection configuration */
    def getOptimalRejection(
        options: Set[Set[Branch]],
        targetF: Branch => Long
    ): Set[Branch] =
      options.toList.minimumByOption(_.map(targetF).sum).getOrElse(Set.empty)

    val (rejectedAsDependents, mergeSet) =
      actualSet.partition(t => lateSet.exists(depends(t, _)))

    /** split merging set into branches without cross dependencies */
    val branches = computeRelatedSets[R](mergeSet, (l, r) => depends(l, r))

    /** map of conflicting branches */
    val conflictMap = computeRelationMap[Set[R]](branches.toList, conflicts)

    /** rejection options that leave only non conflicting branches */
    val rejectionOptions: Set[Rejection] = computeRejectionOptions(conflictMap)

    /** target function for rejection is minimising cost of deploys rejected */
    val rejectionTargetF = (dc: Branch) => dc.map(cost).sum
    val optimalRejection = getOptimalRejection(rejectionOptions, rejectionTargetF)
    val toMerge          = branches diff optimalRejection
    val rejected         = lateSet ++ rejectedAsDependents ++ optimalRejection.flatten

    for {
      allChanges  <- toMerge.toList.flatten.traverse(stateChanges).map(_.combineAll)
      trieActions <- computeTrieActions(baseState, allChanges)
      newState    <- applyTrieActions(baseState, trieActions)
    } yield (newState, rejected)
  }
}
