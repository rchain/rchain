package coop.rchain.v2.validation
import coop.rchain.v2.casper.data.CasperScope

object Rules {

  /**
   * Node is equivocating if there are conflicting messages from the same sender found in the CasperScope.
   * This offence is detected not on block replay but on creation of some future block.
   */
  def findEquivocations[M, S](
      casperScope: CasperScope[M, S]
  )(conflictsMap: CasperScope[M, S] => Map[M, Set[M]], sender: M => S): Map[M, Set[M]] =
    conflictsMap(casperScope).filter { case (m, conflicting) =>
      conflicting.map(sender) contains sender(m)
    }
}
