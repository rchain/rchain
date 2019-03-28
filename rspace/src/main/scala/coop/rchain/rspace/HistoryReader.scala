package coop.rchain.rspace

import coop.rchain.rspace.internal.{Datum, WaitingContinuation}

trait HistoryReader[F[_], C, P, A, K] {
  def getJoins(channel: C): F[List[List[C]]]
  def getData(channel: C): F[List[Datum[A]]]
  def getContinuation(channels: List[C]): F[List[WaitingContinuation[P, K]]]
}
