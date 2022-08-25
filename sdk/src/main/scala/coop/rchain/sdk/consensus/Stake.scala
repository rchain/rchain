package coop.rchain.sdk.consensus

object Stake {

  /**
    * Check if stake prevails threshold of 2/3
    */
  def isSuperMajority(stake: Long, totalStake: Long): Boolean = stake.toDouble / totalStake > 2d / 3
}
