package coop.rchain.v2.casper.data

/**
 * Highest finalized messages.
 */
case class FinalizationFringe[M](v: Set[M])

object FinalizationFringe {

  /**
   * Whether there is finalized message for all known senders participating on a shard found.
   *
   * Complete finalization fringe means that there is finalized state in the network and [[CasperScope]] can be defined.
   */
  def complete[M, S](finalizationFringe: FinalizationFringe[M], knownSenders: Set[S])(
      sender: M => S
  ): Boolean = finalizationFringe.v.map(sender) == knownSenders
}
