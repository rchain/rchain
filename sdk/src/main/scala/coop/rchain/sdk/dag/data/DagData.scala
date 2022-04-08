package coop.rchain.sdk.dag.data

/**
  * Represents field accessors for message and sender.
  *
  * @tparam M represents a message
  * @tparam MId represents a message identifier (message hash)
  * @tparam S represents a sender
  * @tparam SId represents a sender identifier (public key)
  */
trait DagData[M, MId, S, SId] {
  /* Message */

  def mid(s: M): MId

  def seqNum(m: M): Long

  def blockNum(m: M): Long

  def justifications(m: M): Set[MId]

  def sender(m: M): SId

  def bondsMap(m: M): Map[SId, Long]

  /* Sender */

  def sid(s: S): SId
}
