package coop.rchain.casper.merging

import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.models.ListParWithRandom
import coop.rchain.rspace.HotStoreTrieAction
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.HistoryReaderBinary
import coop.rchain.rspace.merger.MergingLogic._
import coop.rchain.rspace.merger.StateChange._
import coop.rchain.rspace.merger._
import coop.rchain.rspace.serializers.ScodecSerialize.DatumB
import coop.rchain.shared.{Log, Stopwatch}
import coop.rchain.rholang.interpreter.merging.RholangMergingLogic.convertToReadNumber
import coop.rchain.rspace.internal.Datum

object ConflictSetMerger {

  /** R is a type for minimal rejection unit */
  def merge[F[_]: Concurrent: Log, R: Ordering](
      actualSet: Set[R],
      lateSet: Set[R],
      depends: (R, R) => Boolean,
      conflicts: (Set[R], Set[R]) => Boolean,
      cost: R => Long,
      stateChanges: R => F[StateChange],
      mergeableChannels: R => NumberChannelsDiff,
      computeTrieActions: (StateChange, NumberChannelsDiff) => F[Vector[HotStoreTrieAction]],
      applyTrieActions: Seq[HotStoreTrieAction] => F[Blake2b256Hash],
      getData: Blake2b256Hash => F[Seq[Datum[ListParWithRandom]]]
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

    def calMergedResult(
        branch: Branch,
        originResult: Map[Blake2b256Hash, Long]
    ): Map[Blake2b256Hash, Long] = {
      val diff = branch.map(mergeableChannels).toList.combineAll
      diff.foldLeft(originResult) {
        case (ba, br) =>
          val result = Math.addExact(ba.getOrElse(br._1, 0L), br._2)
          assert(result >= 0, "merged result negative")
          ba.updated(br._1, result)
      }
    }

    def foldRejection(baseBalance: Map[Blake2b256Hash, Long], branches: Set[Branch]) = {
      val (_, rejected) = branches.foldLeft((baseBalance, Set.empty[Branch])) {
        case ((balances, rejected), deploy) =>
          // TODO come up with a better algorithm to solve below case
          // currently we are accumulating result from some order and reject the deploy once negative result happens
          // which doesn't seem perfect cases below
          //
          // base result 10 and folding the result from order like [-10, -1, 20]
          // which on the second case `-1`, the calculation currently would reject it because the result turns
          // into negative.However, if you look at the all the item view 10 - 10 -1 + 20 is not negative
          try {
            (calMergedResult(deploy, balances), rejected)
          } catch {
            case _: ArithmeticException => (balances, rejected + deploy)
            case _: AssertionError      => (balances, rejected + deploy)
          }
      }
      rejected
    }

    def getMergedResultRejection(
        branches: Set[Branch],
        rejectOptions: Set[Set[Branch]],
        base: Map[Blake2b256Hash, Long]
    ): Set[Set[Branch]] =
      if (rejectOptions.isEmpty) {
        Set(foldRejection(base, branches))
      } else {
        rejectOptions.map {
          case normalRejectOptions =>
            val rejected = foldRejection(base, branches diff normalRejectOptions)
            rejected ++ normalRejectOptions
        }
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

    for {
      baseMergeableChRes <- branches.flatten
                             .map(mergeableChannels)
                             .flatMap(_.keys)
                             .toList
                             .traverse(
                               channelHash =>
                                 convertToReadNumber(getData)
                                   .apply(channelHash)
                                   .map(res => (channelHash, res.getOrElse(0L)))
                             )
                             .map(_.toMap)
      rejectionOptionsWithOverflow = getMergedResultRejection(
        branches,
        rejectionOptions,
        baseMergeableChRes
      )
      optimalRejection = getOptimalRejection(rejectionOptionsWithOverflow, rejectionTargetF)
      rejected         = lateSet ++ rejectedAsDependents ++ optimalRejection.flatten
      toMerge          = branches diff optimalRejection
      r                <- Stopwatch.duration(toMerge.toList.flatten.traverse(stateChanges).map(_.combineAll))

      (allChanges, combineAllChanges) = r

      // All number channels merged
      // TODO: Negative or overflow should be rejected before!
      allMergeableChannels = toMerge.toList.flatten.map(mergeableChannels).combineAll

      r                                 <- Stopwatch.duration(computeTrieActions(allChanges, allMergeableChannels))
      (trieActions, computeActionsTime) = r
      r                                 <- Stopwatch.duration(applyTrieActions(trieActions))
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
