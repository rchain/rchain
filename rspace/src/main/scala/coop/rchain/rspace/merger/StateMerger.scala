package coop.rchain.rspace.merger

import coop.rchain.rspace.history.History
import coop.rchain.rspace.trace.Event

import scala.language.higherKinds

trait StateMerger[F[_]] {

  /**
    * Merge chains of events into some on-chain state (History), resulting in new History.
    *
    * NOTE: this API is supposed to be paired wth conflict detection API and called only from more high level
    * code providing merging capabilities for on-chain execution.
    * Its NOT safe to merge arbitrary event chains into arbitrary states.
    *
    * @param mainState - History to merge events into
    * @param toMerge - sequence of event chains to merge
    * @return merged History
    */
  def merge(mainState: History[F], toMerge: Seq[EventChain[F]]): F[History[F]]
}

/**
  * Sequence of events along with data about state.
  * @param startState start state (History root)
  * @param endState end state (History root)
  * @param events events that transition state from start to end
  */
final case class EventChain[F[_]](
    startState: History[F],
    endState: History[F],
    events: Seq[Event]
)

/**
  * Data changes for a channel
  * @param added items added
  * @param removed items removed
  * @tparam A type of data item: Datum, Continuation or Join
  */
final case class ChannelChange[A](added: Seq[A] = Vector.empty, removed: Seq[A] = Vector.empty)
