package coop.rchain.rspace.history

import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.internal._
import scodec.bits.ByteVector

import scala.language.higherKinds

/**
  * This is similar with [[coop.rchain.rspace.HistoryReader]] but it can only get data from hash instead of object.
  */
trait HistoryHashReader[F[_], C, P, A, K] {
  def getJoins(joinHash: Blake2b256Hash): F[Seq[Seq[C]]]
  def getData(dataHash: Blake2b256Hash): F[Seq[Datum[A]]]
  // data and raw bytes of the data from store
  def getDataWithBytes(dataHash: Blake2b256Hash): F[Seq[(Datum[A], ByteVector)]]
  def getContinuations(
      continuationHash: Blake2b256Hash
  ): F[Seq[WaitingContinuation[P, K]]]
  // continuation and raw bytes of the continuation from store
  def getContinuationsWithBytes(
      continuationHash: Blake2b256Hash
  ): F[Seq[(WaitingContinuation[P, K], ByteVector)]]
}
