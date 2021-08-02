package coop.rchain.rspace.merger

import cats.Parallel
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.trace.{Consume, Produce}
import coop.rchain.shared.Stopwatch
import fs2.Stream

import scala.Function.tupled
import scala.annotation.tailrec

object MergingLogic {

  /** If target depends on source. */
  def depends(target: EventLogIndex, source: EventLogIndex): Boolean = {
    val dependentProduces = (producesCreatedAndNotDestroyed(source) intersect target.producesConsumed)
    val dependentConsumes = (consumesCreatedAndNotDestroyed(source) intersect target.consumesProduced)

    // this should not be the case in real life, but to make function more robust
    (dependentProduces diff (producesCreatedAndNotDestroyed(target))).nonEmpty ||
    (dependentConsumes diff (consumesCreatedAndNotDestroyed(target))).nonEmpty
  }

  /** If two event logs are conflicting. */
  def areConflicting(a: EventLogIndex, b: EventLogIndex): Boolean =
    conflicts(a: EventLogIndex, b: EventLogIndex).nonEmpty

  /** Channels conflicting between a pair of event logs. */
  def conflicts(a: EventLogIndex, b: EventLogIndex): Iterator[Blake2b256Hash] = {

    /**
      * Check #1
      * If the same produce or consume is destroyed in COMM in both branches, this might be a race.
      * All events created in event logs are unique, this match can be identified by comparing case classes.
      *
      * Produce is considered destroyed in COMM if it is not persistent and been consumed without peek.
      * Consume is considered destroyed in COMM when it is not persistent.
      */
    val racesForSameIOEvent = {
      val consumeRaces =
        (a.consumesProduced intersect b.consumesProduced).toIterator.filterNot(_.persistent)
      val produceRaces =
        (a.producesConsumed intersect b.producesConsumed).toIterator.filterNot(_.persistent)

      consumeRaces.flatMap(_.channelsHashes) ++ produceRaces.map(_.channelsHash)
    }

    /**
      * Check #2
      * Events that are created inside branch and has not been destroyed in branch's COMMs
      * can lead to potential COMM during merge.
      */
    val potentialCOMMs = {
      // TODO analyze joins to make less conflicts. Now plain channel intersection treated as a conflict
      def matchFound(consume: Consume, produce: Produce): Boolean =
        consume.channelsHashes contains produce.channelsHash

      // Search for match
      def check(left: EventLogIndex, right: EventLogIndex): Iterator[Blake2b256Hash] = {
        val p = producesCreatedAndNotDestroyed(left)
        val c = consumesCreatedAndNotDestroyed(right)
        p.toIterator
          .flatMap(p => c.toIterator.map((_, p)))
          .filter(tupled(matchFound))
          .map(_._2.channelsHash)
      }

      check(a, b) ++ check(b, a)
    }

    // now we don't analyze joins and declare conflicting cases when produce touch join because applying
    // produces from both event logs might trigger continuation of some join, so COMM event
    val produceTouchBaseJoin =
      (a.producesTouchingBaseJoins.toIterator ++ b.producesTouchingBaseJoins.toIterator)
        .map(_.channelsHash)

    racesForSameIOEvent ++ potentialCOMMs ++ produceTouchBaseJoin
  }

  /** Produce created inside event log. */
  def producesCreated(e: EventLogIndex): Set[Produce] =
    (e.producesLinear ++ e.producesPersistent) diff e.producesCopiedByPeek

  /** Consume created inside event log. */
  def consumesCreated(e: EventLogIndex): Set[Consume] =
    e.consumesLinearAndPeeks ++ e.consumesPersistent

  /** Produces that are created inside event log and not destroyed via COMM inside event log. */
  def producesCreatedAndNotDestroyed(e: EventLogIndex): Set[Produce] =
    ((e.producesLinear diff e.producesConsumed) ++ e.producesPersistent) diff e.producesCopiedByPeek

  /** Consumes that are created inside event log and not destroyed via COMM inside event log. */
  def consumesCreatedAndNotDestroyed(e: EventLogIndex): Set[Consume] =
    e.consumesLinearAndPeeks.diff(e.consumesProduced) ++ e.consumesPersistent

  /** Produces that are affected by event log - locally created + external destroyed. */
  def producesAffected(e: EventLogIndex): Set[Produce] = {
    def externalProducesDestroyed(e: EventLogIndex): Set[Produce] =
      (e.producesConsumed diff producesCreated(e)).filterNot(_.persistent)

    producesCreatedAndNotDestroyed(e) ++ externalProducesDestroyed(e)
  }

  /** Consumes that are affected by event log - locally created + external destroyed. */
  def consumesAffected(e: EventLogIndex): Set[Consume] = {
    def externalConsumesDestroyed(e: EventLogIndex): Set[Consume] =
      (e.consumesProduced diff consumesCreated(e)).filterNot(_.persistent)

    consumesCreatedAndNotDestroyed(e) ++ externalConsumesDestroyed(e)
  }

  /** If produce is copied by peek in one index and originated in another - it is considered as created in aggregate. */
  def combineProducesCopiedByPeek(x: EventLogIndex, y: EventLogIndex): Set[Produce] =
    Seq(x, y)
      .map(_.producesCopiedByPeek)
      .reduce(_ ++ _) diff Seq(x, y).map(producesCreated).reduce(_ ++ _)

  /**
    * Arrange list[v] into map v -> Iterator[v] for items that match predicate.
    * NOTE: predicate here is forced to be non directional.
    * If either (a,b) or (b,a) is true, both relations are recorded as true.
    * TODO: adjust once dependency graph is implemented for branch computing
    */
  def computeRelationMap[A](items: Set[A], relation: (A, A) => Boolean): Map[A, Set[A]] = {
    val init = items.map(_ -> Set.empty[A]).toMap
    items.toList
      .combinations(2)
      .filter {
        case List(l, r) => relation(l, r) || relation(r, l)
      }
      .foldLeft(init) {
        case (acc, List(l, r)) => acc.updated(l, acc(l) + r).updated(r, acc(r) + l)
      }
  }

  /** Given relation map, return iterators of related items. */
  def gatherRelatedSets[A](relationMap: Map[A, Set[A]]): Set[Set[A]] = {
    @tailrec
    def addRelations(toAdd: Set[A], acc: Set[A]): Set[A] = {
      // stop if all new dependencies are already in set
      val next = (acc ++ toAdd)
      val stop = next == acc
      if (stop)
        acc
      else {
        val n = toAdd.flatMap(v => relationMap.getOrElse(v, Set.empty))
        addRelations(n, next)
      }
    }
    relationMap.keySet.map(k => addRelations(relationMap(k), Set(k)))
  }

  def computeRelatedSets[A](items: Set[A], relation: (A, A) => Boolean): Set[Set[A]] =
    gatherRelatedSets(computeRelationMap(items, relation))

  /** Given conflicts map, output possible rejection options. */
  def computeRejectionOptions[A](
      conflictMap: Map[A, Set[A]]
  ): Set[Set[A]] = {
    // Set of rejection paths with corresponding remaining conflicts map
    final case class RejectionOption(rejectedSoFar: Set[A], remainingConflictsMap: Map[A, Set[A]])

    def gatherRejOptions(conflictsMap: Map[A, Set[A]]): Set[RejectionOption] =
      conflictsMap.iterator.map {
        case (_, toReject) =>
          // reject conflicting with keep decision
          val remainingConflictsMap =
            conflictsMap
              .filterKeys(!toReject.contains(_)) // remove rejected key
              .mapValues(_ -- toReject)          // remove rejected amongst values
              .filter { case (_, v) => v.nonEmpty } // remove keys that do not conflict with anything now
          RejectionOption(toReject, remainingConflictsMap)
      }.toSet

    val conflictsOnlyMap = conflictMap.filter {
      // only keys that have conflicts associated should be examined
      case (_, conflicts) => conflicts.nonEmpty
    }
    //     start with rejecting nothing and full conflicts map
    val start = RejectionOption(Set.empty[A], conflictsOnlyMap)
    Stream
      .unfoldLoop(Set(start)) { rejections =>
        val (out, next) = rejections
          .flatMap {
            case RejectionOption(rejectedAcc, remainingConflictsMap) =>
              gatherRejOptions(remainingConflictsMap).map { v: RejectionOption =>
                v.copy(rejectedSoFar = rejectedAcc ++ v.rejectedSoFar)
              }
          }
          // emit rejection option that do not have more conflicts, pass others further into the loop
          .partition { case RejectionOption(_, cMap) => cMap.values.flatten.isEmpty }

        (out.map(_.rejectedSoFar).toList, if (next.nonEmpty) Some(next) else None)
      }
      .flatMap(Stream.emits)
      .toList
      .toSet

//    @tailrec
//    def go(doneAcc: Set[RejectionOption], toDoAcc: Set[RejectionOption]): Set[Set[A]] = {
//      val (done, next) = toDoAcc
//        .flatMap {
//          case RejectionOption(rejectedAcc, remainingConflictsMap) =>
//            gatherRejOptions(remainingConflictsMap).map { v: RejectionOption =>
//              v.copy(rejectedSoFar = rejectedAcc ++ v.rejectedSoFar)
//            }
//        }
//        // emit rejection option that do not have more conflicts, pass others further into the loop
//        .partition { case RejectionOption(_, cMap) => cMap.values.flatten.isEmpty }
//      if (next.isEmpty) done.map(_.rejectedSoFar)
//      else go(doneAcc ++ done, next)
//    }
//    go(Set.empty[RejectionOption], gatherRejOptions(conflictsOnlyMap))
  }
}
