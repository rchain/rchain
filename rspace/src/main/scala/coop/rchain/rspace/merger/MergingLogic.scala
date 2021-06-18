package coop.rchain.rspace.merger

import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.trace.{Consume, Produce}

import scala.Function.tupled
import scala.annotation.tailrec

object MergingLogic {

  /** If target depends on source. */
  def depends(target: EventLogIndex, source: EventLogIndex): Boolean =
    (producesCreatedAndNotDestroyed(source) intersect target.producesConsumed).nonEmpty ||
      (consumesCreatedAndNotDestroyed(source) intersect target.consumesProduced).nonEmpty

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

      val cr = consumeRaces.toList
      val pr = produceRaces.toList
      cr.foreach(c => println(s"Race for consume $c found"))
      pr.foreach(
        p =>
          println(
            s"Race for produce $p found, " +
              s"existsInPreStateA: ${a.producesExistingInPreState.contains(p)}\n" +
              s"existsInPreStateB: ${b.producesExistingInPreState.contains(p)}\n" +
              s"has linear produce record in A: ${a.producesLinear.contains(p)}\n" +
              s"has persistent produce record in A: ${a.producesPersistent.contains(p)}\n" +
              s"has linear produce record in B: ${b.producesLinear.contains(p)}\n" +
              s"has persistent produce record in B: ${b.producesPersistent.contains(p)}\n" +
              s"has peek record in A: ${a.producesPeeked.contains(p)}\n" +
              s"has consume produce record in A: ${a.producesConsumed.contains(p)}\n" +
              s"has peek record in B: ${b.producesPeeked.contains(p)}\n" +
              s"has consume produce record in B: ${b.producesConsumed.contains(p)}\n" +
              s"produce occurrences in a: ${a.producesLinearList.count(_ == p)}\n" +
              s"produce occurrences in b: ${b.producesLinearList.count(_ == p)}\n"
          )
      )

      (cr.flatMap(_.channelsHashes) ++ pr.map(_.channelsHash)).toIterator
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

    val r  = racesForSameIOEvent.toList
    val p  = potentialCOMMs.toList
    val tb = produceTouchBaseJoin.toList
    if (r.nonEmpty) println(s"racesForSameIOEvent: ${r}")
    if (p.nonEmpty) println(s"potentialCOMMs: ${p}")
    if (tb.nonEmpty) println(s"produceTouchBaseJoin: ${tb.toList}")

    (r ++ p ++ tb).toIterator //racesForSameIOEvent ++ potentialCOMMs ++ produceTouchBaseJoin
  }

  /** Produce created inside event log. */
  def producesCreated(e: EventLogIndex): Set[Produce] =
    (e.producesLinear ++ e.producesPersistent) diff e.producesExistingInPreState

  /** Consume created inside event log. */
  def consumesCreated(e: EventLogIndex): Set[Consume] =
    e.consumesLinearAndPeeks ++ e.consumesPersistent

  /** Produces that are created inside event log and not destroyed via COMM inside event log. */
  def producesCreatedAndNotDestroyed(e: EventLogIndex): Set[Produce] =
    ((e.producesLinear diff e.producesConsumed) ++ e.producesPersistent) diff e.producesExistingInPreState

  /** Consumes that are created inside event log and not destroyed via COMM inside event log. */
  def consumesCreatedAndNotDestroyed(e: EventLogIndex): Set[Consume] =
    e.consumesLinearAndPeeks.diff(e.consumesProduced) ++ e.consumesPersistent

  /** Produces that are affected by event log - locally created + external destroyed. */
  def producesAffected(e: EventLogIndex): Set[Produce] = {
    def externalProducesDestroyed(e: EventLogIndex): Set[Produce] =
      e.producesConsumed diff producesCreated(e)

    producesCreatedAndNotDestroyed(e) ++ externalProducesDestroyed(e)
  }

  /** Consumes that are affected by event log - locally created + external destroyed. */
  def consumesAffected(e: EventLogIndex): Set[Consume] = {
    def externalConsumesDestroyed(e: EventLogIndex): Set[Consume] =
      e.consumesProduced diff consumesCreated(e)

    consumesCreatedAndNotDestroyed(e) ++ externalConsumesDestroyed(e)
  }

  /** If produce is copied by peek in one index and originated in another - it is considered as created in aggregate. */
  def combineProducesCopiedByPeek(x: EventLogIndex, y: EventLogIndex): Set[Produce] =
    Seq(x, y)
      .map(_.producesExistingInPreState)
      .reduce(_ ++ _) diff Seq(x, y).map(producesCreated).reduce(_ ++ _)

  /** Arrange list[v] into map v -> Iterator[v] for items that match predicate. */
  def computeRelationMap[A](items: Set[A], relation: (A, A) => Boolean): Map[A, Set[A]] = {
    val init = items.map(_ -> Set.empty[A]).toMap
    items.toList
      .combinations(2)
      .filter {
        case List(l, r) => relation(l, r)
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
        val next = toAdd.flatMap(v => relationMap.getOrElse(v, Set.empty))
        addRelations(next, next)
      }
    }
    relationMap.keySet.map(k => addRelations(relationMap(k), Set(k)))
  }

  def computeRelatedSets[A](items: Set[A], relation: (A, A) => Boolean): Set[Set[A]] =
    gatherRelatedSets(computeRelationMap(items, relation))

  /** Given conflicts map, output possible rejection options. */
  def computeRejectionOptions[A](conflictMap: Map[A, Set[A]]): Set[Set[A]] = {
    @tailrec
    def process(newSet: Set[A], acc: Set[A], reject: Boolean): Set[A] = {
      // stop if all new dependencies are already in set
      val next = if (reject) (acc ++ newSet) else acc
      val stop = if (reject) next == acc else false
      if (stop)
        acc
      else {
        val next = newSet.flatMap(v => conflictMap.getOrElse(v, Set.empty))
        process(next, next, !reject)
      }
    }
    // each rejection option is defined by decision not to reject a key in rejection map
    conflictMap
    // only keys that have conflicts associated should be examined
      .filter { case (_, conflicts) => conflicts.nonEmpty }
      .keySet
      .map(k => process(conflictMap(k), Set.empty, reject = true))
  }
}
