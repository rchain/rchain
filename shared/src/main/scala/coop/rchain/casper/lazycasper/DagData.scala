package coop.rchain.casper.lazycasper

trait DagData[F[_], M, S] {
  def witnessesF: M => F[Map[S, M]]
  def justificationsF: M => F[Map[S, M]]
  def seqNum: M => Long
  def sender: M => S
}
