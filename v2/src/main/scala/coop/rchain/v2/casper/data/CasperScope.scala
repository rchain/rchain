package coop.rchain.v2.casper.data

/**
 * Scope containing all information required for Casper to make decisions: finalized state of the network and all
 * messages that are not finalized.
 *
 * Casper scope is valid only if finalization fringe is complete: finalized messages are found for all known senders.
 *
 * @param latestMessages      Upper boundary, latest messages known. Define the whole [[CasperScope]].
 * @param finalizationFringe  Bottom boundary, set of finalized messages for all known senders.
 * @param conflictScope       The body of the scope - all messages above [[finalizationFringe]].
 */
final case class CasperScope[M, S](
    latestMessages: LatestMessages[M, S],
    finalizationFringe: FinalizationFringe[M, S],
    conflictScope: ConflictScope[M]
) { require(finalizationFringe.v.keySet == latestMessages.v.keySet) }
