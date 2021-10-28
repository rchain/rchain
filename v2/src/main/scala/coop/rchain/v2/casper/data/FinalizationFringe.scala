package coop.rchain.v2.casper.data

/**
 * Highest finalized messages for senders.
 *
 * Unlike [[LatestMessages]], there can be only one finalized message per sender in a fringe,
 * otherwise it would mean finalizing equivocating messages.
 */
final case class FinalizationFringe[M, S](v: Map[S, M])

object FinalizationFringe {

  /**
   * Find messages that are finalized in next compared to prev.
   */
  def diff[M, S](next: FinalizationFringe[M, S], prev: FinalizationFringe[M, S]): Set[M] = ???
}
