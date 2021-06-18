package coop.rchain.casper.merging

import cats.Show
import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.rspace.HotStoreTrieAction
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.merger.MergingLogic._
import coop.rchain.rspace.merger.StateChange._
import coop.rchain.rspace.merger._
import coop.rchain.shared.{Log, Stopwatch}

import scala.collection.mutable

object ConflictSetMerger {

  /** R is a type for minimal rejection unit */
  def merge[F[_]: Concurrent: Log, R](
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
    ): Set[Branch] =
      options.toList.minimumByOption(_.map(targetF).sum).getOrElse(Set.empty)

    val (rejectedAsDependents, mergeSet) =
      actualSet.partition(t => lateSet.exists(depends(t, _)))

    /** split merging set into branches without cross dependencies */
    val (branches, branchesTime) =
      Stopwatch.profile(computeRelatedSets[R](mergeSet, (l, r) => depends(l, r)))

    /** map of conflicting branches */
    val (conflictMap, conflictsMapTime) =
      Stopwatch.profile(computeRelationMap[Set[R]](branches, conflicts))

    val map = mutable.Map.empty[Set[R], Int]
    var cur = 0
    implicit val showR = new Show[Set[R]] {
      override def show(t: Set[R]): String =
        map.get(t) match {
          case Some(i) => i.toString
          case None =>
            cur = cur + 1
            map.update(t, cur)
            cur.toString
        }
    }

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
      overallChanges                    = s"${allChanges.datumChanges.size} D, ${allChanges.kontChanges.size} K, ${allChanges.joinsIndex.size} J"
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
