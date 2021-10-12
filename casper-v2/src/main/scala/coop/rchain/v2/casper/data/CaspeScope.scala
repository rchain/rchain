package coop.rchain.v2.casper.data

/**
 * Scope containing all information required for Casper to make decisions.
 *
 * Scope is defined by [[latestMessages]].
 *
 * @param latestMessages      Upper boundary, latest messages known.
 * @param finalizationFringe  Bottom boundary, set of finalized messages.
 * @param conflictScope       The body of the scope - all messages above and including [[finalizationFringe]].
 */
final case class CasperScope[M, S](
    latestMessages: LatestMessages[M, S],
    finalizationFringe: FinalizationFringe[M],
    conflictScope: ConflictScope[M]
)
