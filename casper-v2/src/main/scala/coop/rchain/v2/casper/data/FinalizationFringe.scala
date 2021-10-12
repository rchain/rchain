package coop.rchain.v2.casper.data

/**
 * Highest finalized messages from all known senders.
 */
case class FinalizationFringe[M](v: Set[M])
