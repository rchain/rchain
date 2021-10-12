package coop.rchain.v2.casper.data

/**
 * Casper scope that is not finalized.
 *
 * All messages above the [[FinalizationFringe]].
 */
case class ConflictScope[M](v: Set[M])
