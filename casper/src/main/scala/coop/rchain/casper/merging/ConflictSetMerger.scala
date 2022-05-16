package coop.rchain.casper.merging

import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.models.ListParWithRandom
import coop.rchain.rspace.HotStoreTrieAction
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.HistoryReaderBinary
import coop.rchain.rspace.merger.EventLogMergingLogic._
import coop.rchain.rspace.merger.StateChange._
import coop.rchain.rspace.merger._
import coop.rchain.rspace.serializers.ScodecSerialize.DatumB
import coop.rchain.shared.{Log, Stopwatch}
import coop.rchain.rholang.interpreter.merging.RholangMergingLogic.convertToReadNumber
import coop.rchain.rspace.internal.Datum

object ConflictSetMerger {

  /** R is a type for minimal rejection unit */
  def merge[F[_]: Concurrent: Log, R: Ordering](
      mergeSet: Set[R],
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
    ): Option[Map[Blake2b256Hash, Long]] = {
      val diff = branch.map(mergeableChannels).toList.combineAll
      diff.foldLeft[Option[Map[Blake2b256Hash, Long]]](Some(originResult)) {
        case (baOpt, br) =>
          baOpt.flatMap {
            case ba =>
              try {
                val result = Math.addExact(ba.getOrElse(br._1, 0L), br._2)
                if (result < 0) {
                  none
                } else {
                  Some(ba.updated(br._1, result))
                }
              } catch {
                case _: ArithmeticException => none
              }
          }
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
          calMergedResult(deploy, balances).fold((balances, rejected + deploy))((_, rejected))
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

    /** split merging set into branches without cross dependencies
      * TODO make dependencies directional, maintain dependency graph. Now if l depends on r or otherwise - it does not matter. */
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
      rejected         = optimalRejection.flatten
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
        s"merge set size ${mergeSet.size}; " +
        s"computed branches (${branches.size}) in ${branchesTime}; " +
        s"conflicts map in ${conflictsMapTime}; " +
        s"rejection options (${rejectionOptions.size}) in ${rejectionOptionsTime}; " +
        s"optimal rejection set size ${optimalRejection.size}; " +
        s"changes combined (${overallChanges}) in ${combineAllChanges}; " +
        s"trie actions (${trieActions.size}) in ${computeActionsTime}; " +
        s"actions applied in ${applyActionsTime}"
      _ <- Log[F].debug(logStr)
    } yield (newState, rejected)
  }
}
