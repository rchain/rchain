package coop.rchain.casper.v2.stcasper

import coop.rchain.casper.v2.stcasper.ConflictsResolver.ConflictResolution

/**
  * Message representing state transition (or just state) from some finalized state common for the whole network
  * to a new state.
  * @tparam U Rejection unit - minimal atomic unit of a state transition.
  */
trait StateMessage[U] {

  /**
    * Part of the state created by the message.
    */
  def proposed: Set[U]

  /**
    * Conflict resolution for message scope.
    */
  def merged: ConflictResolution[U]
}

object StateMessage {

  /**
    * Whether two messages are conflicting. Please use syntax instead.
    */
  def conflicts[U](l: StateMessage[U], r: StateMessage[U]): Boolean =
    //(l.merged.rejectedSet intersect r.proposed).nonEmpty ||
    (l.merged.rejectedSet intersect r.merged.acceptedSet).nonEmpty ||
      //(r.merged.rejectedSet intersect l.proposed).nonEmpty ||
      (r.merged.rejectedSet intersect l.merged.acceptedSet).nonEmpty
}
