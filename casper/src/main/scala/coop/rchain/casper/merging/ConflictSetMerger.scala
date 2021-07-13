package coop.rchain.casper.merging

import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.rspace.HotStoreTrieAction
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.merger.MergingLogic._
import coop.rchain.rspace.merger.StateChange._
import coop.rchain.rspace.merger._
import coop.rchain.shared.{Log, Stopwatch}

object ConflictSetMerger {

  /** R is a type for minimal rejection unit */
  def merge[F[_]: Concurrent: Log, R: Ordering](
      baseState: Blake2b256Hash,
      actualSet: Set[R],
      lateSet: Set[R],
      depends: (R, R) => Boolean,
      conflicts: (Set[R], Set[R]) => Boolean,
      cost: R => Long,
      stateChanges: R => F[StateChange],
      computeTrieActions: (Blake2b256Hash, StateChange) => F[List[HotStoreTrieAction]],
      applyTrieActions: (Blake2b256Hash, Seq[HotStoreTrieAction]) => F[Blake2b256Hash]
  ): F[(Blake2b256Hash, Set[R])] = {

    type Branch = Set[R]

    /** compute optimal rejection configuration */
    def getOptimalRejection(
        options: Set[Set[Branch]],
        targetF: Branch => Long
    ): Set[Branch] = {
      require(
        options.map(_.map(_.head)).size == options.size,
        "Same rejection unit is found in two rejection options. Please report this to code maintainer."
      )
      options.toList
      // reject set with min sum of target function output,
      // if equal value - min size of a branch,
      // if equal size - sorted by head of rejection set option
        .sortBy(b => (b.map(targetF).sum, b.size, b.head.head))
        .headOption
        .getOrElse(Set.empty)
    }

    val (rejectedAsDependents, mergeSet) =
      actualSet.partition(t => lateSet.exists(depends(t, _)))

    /** split merging set into branches without cross dependencies
      * TODO make dependencies directional, maintain dependency graph. Now if l depends on r or otherwise - it does not matter.*/
    val (branches, branchesTime) =
      Stopwatch.profile(computeRelatedSets[R](mergeSet, (l, r) => depends(l, r)))

    /** map of conflicting branches */
    val (conflictMap, conflictsMapTime) =
      Stopwatch.profile(computeRelationMap[Set[R]](branches, conflicts))

    // TODO reject only units that are conflicting + dependent, its not necessary to reject the whole branch
    /** rejection options that leave only non conflicting branches */
    val (rejectionOptions, rejectionOptionsTime) =
      Stopwatch.profile(computeRejectionOptions(conflictMap))

    /** target function for rejection is minimising cost of deploys rejected */
    val rejectionTargetF = (dc: Branch) => dc.map(cost).sum
    val optimalRejection = getOptimalRejection(rejectionOptions, rejectionTargetF)
    val toMerge          = branches diff optimalRejection
    val rejected         = lateSet ++ rejectedAsDependents ++ optimalRejection.flatten

    for {
      r                                 <- Stopwatch.duration(toMerge.toList.flatten.traverse(stateChanges).map(_.combineAll))
      (allChanges, combineAllChanges)   = r
      r                                 <- Stopwatch.duration(computeTrieActions(baseState, allChanges))
      (trieActions, computeActionsTime) = r
      r                                 <- Stopwatch.duration(applyTrieActions(baseState, trieActions))
      (newState, applyActionsTime)      = r
      overallChanges                    = s"${allChanges.datumsChanges.size} D, ${allChanges.kontChanges.size} K, ${allChanges.consumeChannelsToJoinSerializedMap.size} J"
      logStr = s"Merging done: " +
        s"late set size ${lateSet.size}; " +
        s"actual set size ${actualSet.size}; " +
        s"computed branches (${branches.size}) in ${branchesTime}; " +
        s"conflicts map in ${conflictsMapTime}; " +
        s"rejection options (${rejectionOptions.size}) in ${rejectionOptionsTime}; " +
        s"optimal rejection set size ${optimalRejection.size}; " +
        s"rejected as late dependency ${rejectedAsDependents.size}; " +
        s"changes combined (${overallChanges}) in ${combineAllChanges}; " +
        s"trie actions (${trieActions.size}) in ${computeActionsTime}; " +
        s"actions applied in ${applyActionsTime}"
      _ <- Log[F].debug(logStr)
    } yield (newState, rejected)
  }
}
