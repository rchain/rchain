package coop.rchain.rspace.history

import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.internal._

import scala.language.higherKinds

/**
  * This is similar with [[coop.rchain.rspace.HistoryReader]] but it can only get data from hash instead of object.
  */
trait HistoryHashReader[F[_], C, P, A, K] {
  def getJoins(joinHash: Blake2b256Hash): F[Seq[Seq[C]]]
  def getData(dataHash: Blake2b256Hash): F[Seq[Datum[A]]]
  def getContinuations(
      continuationHash: Blake2b256Hash
  ): F[Seq[WaitingContinuation[P, K]]]
}
