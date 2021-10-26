package coop.rchain.v2.casper.data

/**
 * Latest messages from senders.
 *
 * To accommodate equivocation, several latest messages for one sender are possible.
 */
final case class LatestMessages[M, S](v: Map[S, Set[M]])
