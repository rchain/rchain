package coop.rchain.rspace.merger.instances

import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.shared.syntax._
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.internal.Datum
import coop.rchain.rspace.merger.EventLogIndex
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import coop.rchain.store.LazyKeyValueCache

import scala.collection.Seq
import scala.collection.immutable.Set

object EventsIndexConflictDetectors {

  /**
    * There are 2 cases when the channel is considered conflicting.
    *
    *   1. If the same produce or consume is destroyed in COMM in both branches, this might be a race.
    *   All events created in event logs are unique, this match can be identified by comparing case classes.
    *
    *   Produce is considered destroyed in COMM if it is not persistent and been consumed without peek.
    *   Consume is considered destroyed in COMM when it is not persistent.
    *
    *   2. Events that are created inside branch and has not been destroyed in branch's COMMs
    *   can lead to potential COMM during merge.
    *   NOTE: this case requires special care about peek, as active event can be inherited from base state because
    *   of peek issue.
    *
    * @return set of channels that are conflicting.
    */
  def findConflicts[F[_]: Concurrent: Log, A, C](
      main: EventLogIndex,
      merge: EventLogIndex,
      // TODO remove baseDataReader, required only for peek workaround
      baseDataReader: LazyKeyValueCache[F, Blake2b256Hash, Seq[Datum[A]]],
      baseJoinReader: LazyKeyValueCache[F, Blake2b256Hash, Seq[Seq[C]]]
  ): F[Set[Blake2b256Hash]] = {

    // Check #1
    val biCommedConsume = merge.consumesDestroyed
      .intersect(main.consumesDestroyed)
    val biCommedProduce = merge.producesDestroyed
      .intersect(main.producesDestroyed)

    val racesForSameConsume =
      biCommedConsume.filterNot(_.persistent).flatMap(_.channelsHashes)
    val racesForSameProduce =
      biCommedProduce.filterNot(_.persistent).map(_.channelsHash)

    // Check #2
    val commOnMergeCheck = {
      // Check is performed by examining active produces of both branches against consumes of other branch
      def check(left: EventLogIndex, right: EventLogIndex): List[fs2.Stream[F, Blake2b256Hash]] = {
        val leftProdActive  = left.producesLinear.diff(left.producesDestroyed) ++ left.producesPersistent
        val rightConsActive = right.consumesLinearAndPeeks.diff(right.consumesDestroyed) ++ right.consumesPersistent

        leftProdActive.toList.map(
          p =>
            fs2.Stream
              .eval({
                // Find consume that waits for data on this channel in opposite branch.
                // Consume found can actually not match if it is a join, but for now we treat all such cases as conflicting.
                // TODO analyze joins to make less conflicts
                val matchingConsume =
                  rightConsActive.find(_.channelsHashes.contains(p.channelsHash))

                // As peek is not atomic, when Produce is peaked - exactly the same produce is created in event log.
                // Because of this event log cannot reliably tell whether produce is originated from base state on top
                // of which this event is created, of copied via peek.
                // We can do some tricks to check whether produce is created by a peek, but this logic is not clear yet.
                // So its safer to just read from base for now.
                val produceFromBase =
                  baseDataReader.get(p.channelsHash).map(_.exists(_.source == p))

                val commBetweenBranches = matchingConsume.nonEmpty.pure[F] &&^ produceFromBase.not

                // In addition, it has to be checked if active produces match some join that is inherited from base state
                val touchingBaseJoin = baseJoinReader.get(p.channelsHash).map(_.exists(_.size > 1))

                for {
                  conflict <- commBetweenBranches ||^ touchingBaseJoin
                } yield (p.channelsHash, conflict)
              })
              .filter {
                case (_, conflict) => conflict
              }
              .map(_._1)
        )
      }

      check(main, merge) ++ check(merge, main)
    }

    for {
      commsBetweenBranches <- fs2.Stream
                               .emits(commOnMergeCheck)
                               .parJoinProcBounded
                               .compile
                               .toList

      racesForSameEvent = racesForSameProduce ++ racesForSameConsume

      _ <- Log[F]
            .info(s"${racesForSameEvent.size} conflicts with base found")
            .whenA(racesForSameEvent.nonEmpty)
      _ <- Log[F]
            .info(s"${commsBetweenBranches.size} potential COMMs between branches found")
            .whenA(commsBetweenBranches.nonEmpty)
    } yield (commsBetweenBranches ++ racesForSameEvent).toSet
  }
}
