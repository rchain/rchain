package coop.rchain.pcasper.finalization.`lazy`

/** Data about DAG structure required for lazy finalization algorithm. */
trait DagData[F[_], M, S] {
  // Witness of a target from a sender is the first message from senders that have the target in the view.
  def witnessesF: M => F[Map[S, M]]
  // Justifications are latest messages seen by target
  def justificationsF: M => F[Map[S, M]]
  // Sequence number of a message produced by sender.
  def seqNum: M => Long
  // Sender of a message
  def sender: M => S
}
