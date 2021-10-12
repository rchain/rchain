package coop.rchain.v2.casper.data

/**
 * Justifications of the message.
 *
 * Unlike [[LatestMessages]], there can be only one justification message per sender,
 * as equivocating justifications are not allowed.
 */
final case class Justifications[M, S](v: Map[S, M])
