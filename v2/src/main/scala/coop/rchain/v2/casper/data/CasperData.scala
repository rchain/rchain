package coop.rchain.v2.casper.data

import coop.rchain.v2.casper.stcasper.StateMessage

/**
 * Properties that Casper requires a message to have to be able to operate.
 * This is aggregate class having all data required to [[coop.rchain.v2.casper.DependencyGraph]] and
 * [[coop.rchain.v2.casper.SafetyOracle]].
 *
 * TODO bondsMap won't change much, so we can store the whole bonds maps in some table and have here only index.
 */
case class CasperData[M, S, U](
    justifications: Set[M],
    sender: S,
    seqNum: Int,
    stateMetadata: StateMessage[U],
    bondsMap: Map[S, Long]
)
