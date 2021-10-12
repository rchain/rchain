package coop.rchain.v2.casper.data

/**
 * Latest messages from all known senders.
 *
 * As equivocation is possible, there can be several latest messages from one sender with the same sequence number.
 * @param v        Messages map.
 * @param seqNum   Sequence number of a message.
 * @tparam M       Type for message.
 * @tparam S       Type for message sender.
 */
final case class LatestMessages[M, S](v: Map[S, Set[M]])
