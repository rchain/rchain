package coop.rchain.rspace.pure

import coop.rchain.rspace.internal.{Datum, Row, WaitingContinuation}

import scala.collection.immutable.Seq

/** The interface for the underlying store
  *
  * @tparam F a type constructor representing an effect (for example `ReaderT[F, LMDBContext, ?]`)
  * @tparam C a type representing a channel
  * @tparam P a type representing a pattern
  * @tparam A a type representing an arbitrary piece of data
  * @tparam K a type representing a continuation
  */
trait Store[F[_], C, P, A, K] {

  /**
    * The type of transactions
    */
  type T

  def createTxnRead(): F[T]

  def createTxnWrite(): F[T]

  def withTxn[R](txn: T)(f: T => R): R

  /**
    * The type of hashes
    */
  type H

  def hashChannels(channels: Seq[C]): F[H]

  def getChannels(txn: T, channelsHash: H): F[Seq[C]]

  /* Data */

  def removeDatum(txn: T, channels: Seq[C], index: Int): F[Unit]

  def removeDatum(txn: T, channel: C, index: Int): F[Unit]

  def putDatum(txn: T, channels: Seq[C], datum: Datum[A]): F[Unit]

  def getData(txn: T, channels: Seq[C]): F[Seq[Datum[A]]]

  /* WaitingContinuations */

  def removeWaitingContinuation(txn: T, channels: Seq[C], index: Int): F[Unit]

  def putWaitingContinuation(txn: T,
                             channels: Seq[C],
                             continuation: WaitingContinuation[P, K]): F[Unit]

  def getWaitingContinuation(txn: T, channels: Seq[C]): F[Seq[WaitingContinuation[P, K]]]

  /* Joins */

  def removeJoin(txn: T, channel: C, channels: Seq[C]): F[Unit]

  def removeAllJoins(txn: T, channel: C): F[Unit]

  def putJoin(txn: T, channel: C, channels: Seq[C]): F[Unit]

  def getJoin(txn: T, channel: C): F[Seq[Seq[C]]]

  /* Utility */

  def removeAll(txn: T, channels: Seq[C]): F[Unit]

  def toMap: F[Map[Seq[C], Row[P, A, K]]]

  /* Lifecycle */

  def close(): F[Unit]
}
