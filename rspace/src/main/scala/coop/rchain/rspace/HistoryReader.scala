package coop.rchain.rspace

import coop.rchain.rspace.internal.{Datum, WaitingContinuation}

trait HistoryReader[F[_], C, P, A, K] {
  def getJoins(channel: C): F[Seq[Seq[C]]]
  def getData(channel: C): F[Seq[Datum[A]]]
  def getContinuations(channels: Seq[C]): F[Seq[WaitingContinuation[P, K]]]
}
