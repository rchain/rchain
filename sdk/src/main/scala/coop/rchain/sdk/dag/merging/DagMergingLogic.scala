package coop.rchain.sdk.dag.merging

import cats.Order
import cats.syntax.all._
import coop.rchain.sdk.dag.merging.data.Merge

import scala.collection.compat.immutable.LazyList
import scala.collection.immutable.Map
import scala.collection.mutable
import scala.math.Numeric.LongIsIntegral

object DagMergingLogic {

  /** All items in dependency chains. */
  def withDependencies[D](of: Set[D], dependencyMap: Map[D, Set[D]]): Set[D] = {
    def next(curOpt: Option[Set[D]]): Option[Set[D]] = curOpt.flatMap { c =>
      val n = c.flatMap(dependencyMap.getOrElse(_, Set()))
      n.nonEmpty.guard[Option].as(n)
    }
    LazyList.iterate(of.some)(next).takeWhile(_.nonEmpty).flatten.flatten.toSet
  }

  /** Deploys incompatible with finalized body. */
  def incompatibleWithFinal[D](
      acceptedFinally: Set[D],
      rejectedFinally: Set[D],
      conflictsMap: Map[D, Set[D]],
      dependencyMap: Map[D, Set[D]]
  ): Set[D] =
    acceptedFinally.flatMap(conflictsMap) ++ rejectedFinally.flatMap(dependencyMap)

  /** Split the scope into non overlapping partitions, greedily allocating intersecting chunks to bigger view. */
  def partitionScope[D: Ordering](views: Seq[Set[D]]): Seq[Set[D]] = {
    val r = LazyList.unfold(views) {
      case (head @ seen) +: tail => (head, tail.map(_ -- seen)).some
      case Seq()                 => none[(Set[D], List[Set[D]])]
    }
    r.toList
  }

  def computeRelationMap[D](
      targetSet: Set[D],
      sourceSet: Set[D],
      relation: (D, D) => Boolean,
      directed: Boolean
  ): Map[D, Set[D]] =
    targetSet.iterator
      .flatMap(t => sourceSet.map((t, _)))
      .foldLeft(mutable.Map.empty[D, Set[D]]) {
        case (acc, (target, source)) =>
          if (relation(target, source) && target != source) {
            acc.update(source, acc.get(source).map(_ + target).getOrElse(Set(target)))
            if (!directed)
              acc.update(target, acc.get(target).map(_ + source).getOrElse(Set(source)))
          }
          acc
      }
      .toMap

  /** Build conflicts map. */
  def computeConflictsMap[D](
      setA: Set[D],
      setB: Set[D],
      conflicts: (D, D) => Boolean
  ): Map[D, Set[D]] =
    computeRelationMap(setA, setB, conflicts, false)

  /**
    * Build dependency map.
    * @param targetSet items that might depend on source set
    * @param sourceSet items that target set might depend on
    * @param depends dependency predicate
    */
  def computeDependencyMap[D](
      targetSet: Set[D],
      sourceSet: Set[D],
      depends: (D, D) => Boolean
  ): Map[D, Set[D]] =
    computeRelationMap(targetSet, sourceSet, depends, true)

  /** Compute branches of depending items. */
  def computeBranches[D: Ordering](target: Set[D], depends: (D, D) => Boolean): Map[D, Set[D]] = {
    val dependencyMap = computeDependencyMap(target, target, depends)
    dependencyMap.foldLeft(dependencyMap) {
      case (acc, (root, depending)) =>
        val rootDependencies = acc.collect { case (k, v) if v.contains(root) => k }
        if (rootDependencies.nonEmpty)
          acc - root |+| rootDependencies.map(_ -> (depending + root)).toMap
        else acc
    } ++ (target -- dependencyMap.flatMap { case (k, v) => v + k }).map(_ -> Set.empty[D])
  }

  /**
    * Compute branches of depending items that do not intersect.
    * Partitioning done via [[partitionScopeBiggestFirst]].
    */
  def computeGreedyNonIntersectingBranches[D: Ordering](
      target: Set[D],
      depends: (D, D) => Boolean
  ): Seq[Set[D]] = {
    val concurrentRoots = computeBranches(target, depends)
    val sorted =
      concurrentRoots.toList.sortBy { case (k, v) => (-v.size, k) }.map { case (k, v) => v + k }
    partitionScope(sorted).toList
  }

  /** Lowest fringe across number of fringes. */
  def lowestFringe[B: Ordering](fringes: Set[Set[B]], height: B => Long): Set[B] = {
    require(fringes.forall(_.nonEmpty), "Fringe has to have at least 1 element.")
    require(fringes.nonEmpty, "Cannot compute lowest fringe on empty set.")
    fringes.minBy { f =>
      val minBlock = f.minBy(b => (height(b), b))
      (height(minBlock), minBlock)
    }
  }

  /** All items in the conflict scope. */
  def conflictScope[B](latestMessages: Set[B], fringeMessages: Set[B], seen: B => Set[B]): Set[B] =
    latestMessages ++ latestMessages.flatMap(seen) -- fringeMessages -- fringeMessages.flatMap(seen)

  /** All items in the final scope. */
  def finalScope[B: Ordering](
      latestFringe: Set[B],
      lowestFringe: Set[B],
      seen: B => Set[B]
  ): Set[B] =
    latestFringe.flatMap(seen) -- lowestFringe.flatMap(seen) ++ latestFringe

  /** Relation map sufficient for merge set. */
  def computeRelationMapForMergeSet[D](
      conflictSet: Set[D],
      finalSet: Set[D],
      conflicts: (D, D) => Boolean,
      depends: (D, D) => Boolean
  ): (Map[D, Set[D]], Map[D, Set[D]]) = {
    val conflictsMap = computeConflictsMap(conflictSet, finalSet, conflicts) ++
      computeConflictsMap(conflictSet, conflictSet, conflicts)
    val dependencyMap = computeDependencyMap(conflictSet, finalSet, depends)
    (conflictsMap, dependencyMap)
  }

  // TODO this is o(2^n) algorithm (see [[MergingBenchmarkSpec]]), another that scales should be developed instead
  def computeRejectionOptions[D](conflictsMap: Map[D, Set[D]]): Set[Set[D]] = {
    def step(a: D, rjAcc: Set[D], acAcc: Set[D]): (Set[D], Set[D], Set[D]) = {
      val newRjAcc      = rjAcc ++ conflictsMap(a)
      val newAcAcc      = acAcc + a
      val nextAcOptions = conflictsMap.keySet -- newRjAcc -- newAcAcc
      (nextAcOptions, newRjAcc, newAcAcc)
    }
    val init = conflictsMap.keySet.map(k => (k, Set.empty[D], Set(k)))
    LazyList
      .unfold[(Set[Set[D]]), Set[(D, Set[D], Set[D])]](init) { x =>
        val (done, continue) = x
          .map((step _).tupled)
          .map {
            case (nextAccept, rjAcc, acAcc) =>
              val n    = nextAccept.map((_, rjAcc, acAcc))
              val done = n.isEmpty
              (n, done.guard[Option].as(rjAcc))
          }
          .partition { case (_, rjDone) => rjDone.isDefined }
        val end  = done.isEmpty && continue.isEmpty
        val out  = done.map(_._2.get)
        val next = continue.flatMap(_._1)
        (!end).guard[Option].as((out, next))
      }
      .flatten
      .toSet
  }

  /** Compute optimal rejection according to the target function. */
  def computeOptimalRejection[D: Ordering](
      options: Set[Set[D]],
      targetF: D => Long
  ): Set[D] = {
    implicit val ordD = Order.fromOrdering[D]
    options.toList
      .minimumByOption { rj =>
        (rj.map(targetF).sum, rj.size, rj.toList.min)
      }
      .getOrElse(Set.empty[D])
  }

  /** Compute rejections options extended with rejections that have to be made due to mergeable value overflow. */
  def addMergeableOverflowRejections[D, CH](
      conflictSet: Set[D],
      rejectOptions: Set[Set[D]],
      initMergeableValues: Map[CH, Long],
      mergeableDiffs: Map[D, Map[CH, Long]]
  ): Set[Set[D]] = {

    def calMergedResult(
        deploy: D,
        initMergeableValues: Map[CH, Long]
    ): Option[Map[CH, Long]] = {
      val diff = mergeableDiffs.getOrElse(deploy, Map())
      diff.foldLeft(initMergeableValues.some) {
        case (accOpt, (channel, change)) =>
          accOpt.flatMap { acc =>
            try {
              val result = Math.addExact(acc.getOrElse(channel, 0L), change)
              if (result < 0) none else Some(acc.updated(channel, result))
            } catch {
              case _: ArithmeticException => none
            }
          }
      }
    }

    def foldRejection(baseBalance: Map[CH, Long], toMerge: Set[D]): Set[D] = {
      // Sort by sum of absolute diffs
      val sorted = toMerge.toList.sortBy { d =>
        mergeableDiffs.get(d).map(_.values.map(Math.abs).sum).getOrElse(Long.MinValue)
      }
      val (_, rejected) = sorted.foldLeft((baseBalance, Set.empty[D])) {
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

    if (rejectOptions.isEmpty) {
      Set(foldRejection(initMergeableValues, conflictSet))
    } else {
      rejectOptions.map { normalRejectOptions =>
        normalRejectOptions ++ foldRejection(
          initMergeableValues,
          conflictSet diff normalRejectOptions
        )
      }
    }
  }

  /** Find deploys to reject from conflict set. */
  def resolveConflictSetFlat[D: Ordering, CH](
      conflictSet: Set[D],
      dependencyMap: Map[D, Set[D]],
      conflictsMap: Map[D, Set[D]],
      cost: D => Long,
      mergeableDiffs: Map[D, Map[CH, Long]],
      initMergeableValues: Map[CH, Long]
  ): Set[D] = {
    // conflict map accounting for dependencies
    val fullConflictsMap = conflictsMap.mapValues(vs => vs ++ withDependencies(vs, dependencyMap))
    // find rejection combinations possible
    val rejectionOptions = computeRejectionOptions(fullConflictsMap)
    // add to rejection options rejections caused by mergeable channels overflow
    val mergeableOverflowRejectionOptions = addMergeableOverflowRejections(
      conflictSet,
      rejectionOptions,
      initMergeableValues,
      mergeableDiffs
    )
    // find optimal rejection
    computeOptimalRejection(mergeableOverflowRejectionOptions, cost)
  }

  /** Across messages find which views are altered by rejections. */
  def computeAltered[B, D](
      messages: Set[B],
      seen: B => Set[B],
      rejections: Set[D],
      deploys: B => Set[D]
  ): (Vector[(B, Set[D])], Vector[(B, Set[D])]) = {
    val (alteredI, notAlteredI) = messages.iterator
      .map { case t => t -> (seen(t) + t).flatMap(deploys) }
      .map { case v @ (_, view) => (v, view intersect rejections) }
      .partition { case (_, x) => x.nonEmpty }
    val (altered, _)    = alteredI.toVector.unzip
    val (notAltered, _) = notAlteredI.toVector.unzip
    (notAltered, altered)
  }

  /** Resolve conflicts through folding branches keeping head immutable and rejecting from tail. */
  def resolveBranchesInOrder[D](
      branches: Seq[Set[D]],
      dependencyMap: Map[D, Set[D]],
      conflictsMap: Map[D, Set[D]]
  ): (LazyList[Set[D]], LazyList[Set[D]]) =
    LazyList
      .unfold(branches) {
        case head +: tail =>
          val (remainder, rejected) = tail.map { view =>
            val inc     = incompatibleWithFinal(head, Set(), conflictsMap, dependencyMap)
            val toClear = withDependencies(inc, dependencyMap)
            (view -- toClear, toClear)
          }.unzip
          ((head, rejected.flatten.toSet), remainder).some
        case Seq() => none[((Set[D], Set[D]), Seq[Set[D]])]
      }
      .unzip

  /**
    * Resolve conflict set by reusing merge done by one of the tips.
    * If all tips states are altered by finalized rejections, None is returned.
    */
  def resolveConflictSetIntoTip[B: Ordering, D: Ordering, CH, S](
      latestMessages: Map[B, S],
      conflictScope: Set[B],
      enforceRejected: Set[D], // required to find out which tip is not altered
      seen: B => Set[B],
      deploys: B => Set[D],
      dependencyMap: Map[D, Set[D]],
      conflictsMap: Map[D, Set[D]],
      mergeableDiffs: Map[D, Map[CH, Long]],
      initMergeableValues: Map[CH, Long]
  ): Option[Merge[S, D]] = {
    val (notAltered, altered) =
      computeAltered(latestMessages.keySet, seen, enforceRejected, deploys)
    if (notAltered.isEmpty) none[Merge[S, D]]
    else {
      val base +: rem = altered.sortBy { case (_, v) => -v.size }
      val toMerge = (base +: (rem ++ notAltered).sortBy { case (_, v) => -v.size }).map {
        case (k, v) => deploys(k) ++ v
      }
      val toResolve            = partitionScope(toMerge)
      val (branches, rejected) = resolveBranchesInOrder(toResolve, dependencyMap, conflictsMap)
      val rejectionsTotal      = enforceRejected ++ rejected.flatten
      val rejectionsWithOverflow = addMergeableOverflowRejections(
        conflictScope.flatMap(deploys),
        Set(rejectionsTotal),
        initMergeableValues,
        mergeableDiffs
      )
      Merge(
        latestMessages(base._1),
        branches.drop(1).toSet.flatten,
        rejectionsWithOverflow.flatten
      ).some
    }
  }

  /** Compute merge for the DAG. */
  def mergeDag[B: Ordering, D: Ordering, CH, S](
      // DAG
      latestMessages: Set[B],
      seen: B => Set[B],
      // finalization
      latestFringe: Set[B],
      fringeState: S,
      acceptedFinally: Set[D],
      rejectedFinally: Set[D],
      // deploys
      deploysIndex: Map[B, Set[D]],
      cost: D => Long,
      // relations
      conflictsMap: Map[D, Set[D]],
      dependencyMap: Map[D, Set[D]],
      // support for mergeable
      mergeableDiffs: Map[D, Map[CH, Long]],
      initMergeableValues: Map[CH, Long]
  ): Merge[S, D] = {
    val enforceRejected = withDependencies(
      incompatibleWithFinal(acceptedFinally, rejectedFinally, conflictsMap, dependencyMap),
      dependencyMap
    )
    // conflict set without deploys conflicting with finalization
    val conflictSet           = conflictScope(latestMessages, latestFringe, seen).flatMap(deploysIndex)
    val conflictSetCompatible = conflictSet -- enforceRejected
    // resolveConflictSetIntoTip can be used here instead
    val resolved = resolveConflictSetFlat(
      conflictSetCompatible,
      dependencyMap,
      conflictsMap,
      cost,
      mergeableDiffs,
      initMergeableValues
    )
    Merge(fringeState, conflictSetCompatible -- resolved, resolved ++ enforceRejected)
  }
}
