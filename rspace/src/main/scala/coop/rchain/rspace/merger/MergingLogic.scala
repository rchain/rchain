package coop.rchain.rspace.merger

import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.trace.{Consume, Produce}

import scala.Function.tupled

object MergingLogic {

  /** if target depends on source */
  def depends(target: EventLogIndex, source: EventLogIndex): Boolean =
    (producesActive(source) intersect target.producesConsumed).nonEmpty ||
      (consumesActive(source) intersect target.consumesProduced).nonEmpty

  /** if two event logs are conflicting */
  def areConflicting(a: EventLogIndex, b: EventLogIndex): Boolean =
    conflicts(a: EventLogIndex, b: EventLogIndex).nonEmpty

  /** Channels conflicting between a pair of event logs */
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
      def equal[A](as: Set[A], bs: Set[A]): Iterator[A] =
        as.iterator
          .flatMap(a => bs.iterator.map(b => (a, b)))
          .filter(tupled((l, r) => l == r))
          .map(_._1)

      val consumeRaces =
        equal[Consume](a.consumesProduced, b.consumesProduced).filterNot(_.persistent)
      val produceRaces =
        equal[Produce](a.producesConsumed, b.producesConsumed).filterNot(_.persistent)

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
        val p = producesActive(left)
        val c = consumesActive(right)
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

  /** produce created inside event log */
  def producesCreated(e: EventLogIndex): Set[Produce] =
    (e.producesLinear ++ e.producesPersistent) diff e.producesCopiedByPeek

  /** consume created inside event log */
  def consumesCreated(e: EventLogIndex): Set[Consume] = e.consumesLinearAndPeeks

  /** produces that are created inside event log and not destroyed via COMM inside event log */
  def producesActive(e: EventLogIndex): Set[Produce] =
    ((e.producesLinear diff e.producesConsumed) ++ e.producesPersistent) diff e.producesCopiedByPeek

  /** consumes that are created inside event log and not destroyed via COMM inside event log */
  def consumesActive(e: EventLogIndex): Set[Consume] =
    e.consumesLinearAndPeeks.diff(e.consumesProduced) ++ e.consumesPersistent

  /** produces that are affected by event log - locally created + external destroyed */
  def producesAffected(e: EventLogIndex): Set[Produce] = {
    def externalProducesDestroyed(e: EventLogIndex): Set[Produce] =
      e.producesConsumed diff producesCreated(e)

    producesActive(e) ++ externalProducesDestroyed(e)
  }

  /** consumes that are affected by event log - locally created + external destroyed */
  def consumesAffected(e: EventLogIndex): Set[Consume] = {
    def externalConsumesDestroyed(e: EventLogIndex): Set[Consume] =
      e.consumesProduced diff consumesCreated(e)

    consumesActive(e) ++ externalConsumesDestroyed(e)
  }
}
