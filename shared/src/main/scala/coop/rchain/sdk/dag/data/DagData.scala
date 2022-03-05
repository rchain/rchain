package coop.rchain.sdk.dag.data

trait DagData[M, S] {
  /* Message */

  def seqNum(m: M): Long

  def blockNum(m: M): Long

  // TODO: this is not possible because message has IDs
  def justifications(m: M): Set[M]

  // TODO: this is not possible because message has IDs
  def sender(m: M): S

  def bondsMap(m: M): Map[S, Long]

  /* Sender */

  def stake(s: S): Long
}
