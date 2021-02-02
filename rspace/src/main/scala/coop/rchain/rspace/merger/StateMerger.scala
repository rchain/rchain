package coop.rchain.rspace.merger

import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.trace.Event

import scala.language.higherKinds

trait StateMerger[F[_]] {

  /**
    * StateMerger is always working like a situation below
    *
    *         mergedState
    *       /         \
    *  mainState      mergingState
    *        \       /
    *          baseState
    *
    * `eventLogs` are the eventLogs happened in mergingState which are not conflict with
    * the eventLogs happened in mainState. The non-conflict
    *
    * baseState is needed here for getting the changes in mergingState. The logic here is that
    * the non-conflict changes in mergingState from baseState is something like `changes = mergingState - baseState`.
    *
    * Supposed that the data in channel C are situation like below:
    *
    *             mergedState
    *       /                     \
    *  mainState= C-> Seq(a, b)      mergingState= C->Seq(a, b, c)
    *        \                    /
    *          baseState = C->Seq(a, b)
    *
    *  The mergedState should be `mergingState - baseState + mainState = Seq(a,b,c)`
    *
    * The target of [[merge]] is to generated the `mergedState` above which merge the mainState and mergingState
    * @param baseState
    * @param mainState
    * @param toMerge - sequence of tuple
    *                (
    *                merging state hash,
    *                events from eventlog that made a difference between base and merging
    *                )
    * @return
    */
  def merge(
      mainState: Blake2b256Hash,
      toMerge: Seq[EventChain]
  ): F[Blake2b256Hash]
}

final case class EventChain(
    startState: Blake2b256Hash,
    endState: Blake2b256Hash,
    events: Seq[Event]
)
