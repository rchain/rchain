package coop.rchain.rspace.merger

import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.trace.{Consume, Produce}
import fs2.Stream

import scala.Function.tupled
import scala.annotation.tailrec

/** Logic for merging event logs. */
object EventLogMergingLogic {

  /**
    * Map used to represent mergeable (numeric) channels with intermediate values
    */
  type NumberChannelsEndVal = Map[Blake2b256Hash, Long]

  type NumberChannelsDiff = Map[Blake2b256Hash, Long]

  /** If target depends on source. */
  def depends(target: EventLogIndex, source: EventLogIndex): Boolean = {
    val producesSource = producesCreatedAndNotDestroyed(source) diff source.producesMergeable
    val producesTarget = target.producesConsumed diff source.producesMergeable

    val consumesSource = consumesCreatedAndNotDestroyed(source)
    val consumesTarget = target.consumesProduced

    val producesDepends = producesSource intersect producesTarget
    val consumesDepends = consumesSource intersect consumesTarget

    producesDepends.nonEmpty || consumesDepends.nonEmpty
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
      *
      * If produces/consumes are mergeable in both indices, they are not considered as conflicts.
      */
    val racesForSameIOEvent = {
      val sharedConsumes    = a.consumesProduced intersect b.consumesProduced
      val mergeableConsumes = a.consumesMergeable intersect b.consumesMergeable
      val consumeRaces      = (sharedConsumes diff mergeableConsumes).toIterator.filterNot(_.persistent)

      val sharedProduces    = a.producesConsumed intersect b.producesConsumed
      val mergeableProduces = a.producesMergeable intersect b.producesMergeable
      val produceRaces      = (sharedProduces diff mergeableProduces).toIterator.filterNot(_.persistent)

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
}
