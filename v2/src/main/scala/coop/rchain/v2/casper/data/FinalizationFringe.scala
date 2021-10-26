package coop.rchain.v2.casper.data

/**
 * Highest finalized messages for senders.
 *
 * Unlike [[LatestMessages]], there can be only one finalized message per sender in a fringe,
 * otherwise it would mean finalizing equivocating messages.
 */
final case class FinalizationFringe[M, S](v: Map[S, M])
