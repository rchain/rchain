package coop.rchain.rspace

import cats._
import coop.rchain.rspace.internal.{Datum, WaitingContinuation}

trait HistoryReader[F[_], C, P, A, K] {
  def getJoins(channel: C): F[List[List[C]]]
  def getData(channel: C): F[List[Datum[A]]]
  def getContinuations(channels: List[C]): F[List[WaitingContinuation[P, K]]]
}
