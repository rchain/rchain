package coop.rchain.sdk.casper

object Stake {

  /**
    * Check if stake prevails threshold of 2/3
    */
  def prevails(stake: Long, totalStake: Long): Boolean = 3 * stake >= 2 * totalStake

  /**
    * Alias for ![[prevails]]
    */
  def notPrevails(stake: Long, totalStake: Long): Boolean = !prevails(stake, totalStake)
}
