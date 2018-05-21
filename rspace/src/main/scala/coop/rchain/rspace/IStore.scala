package coop.rchain.rspace

import scala.collection.immutable.Seq
import coop.rchain.rspace.internal._

/** The interface for the underlying store
  *
  * @tparam C a type representing a channel
  * @tparam P a type representing a pattern
  * @tparam A a type representing an arbitrary piece of data
  * @tparam K a type representing a continuation
  */
trait IStore[C, P, A, K] {

  /**
    * The type of hashes
    */
  private[rspace] type H

  /**
    * The type of transactions
    */
  private[rspace] type T

  private[rspace] def hashChannels(channels: Seq[C])(implicit sc: Serialize[C]): H

  private[rspace] def getChannels(txn: T, channelsHash: H): Seq[C]

  private[rspace] def removeDatum(txn: T, channels: Seq[C], index: Int): Unit

  private[rspace] def createTxnRead(): T

  private[rspace] def createTxnWrite(): T

  private[rspace] def withTxn[R](txn: T)(f: T => R): R

  private[rspace] def putDatum(txn: T, channels: Seq[C], datum: Datum[A]): Unit

  private[rspace] def putWaitingContinuation(txn: T,
                                             channels: Seq[C],
                                             continuation: WaitingContinuation[P, K]): Unit

  private[rspace] def getData(txn: T, channels: Seq[C]): Seq[Datum[A]]

  private[rspace] def getWaitingContinuation(txn: T,
                                             channels: Seq[C]): Seq[WaitingContinuation[P, K]]

  private[rspace] def removeDatum(txn: T, channel: C, index: Int): Unit

  private[rspace] def removeWaitingContinuation(txn: T, channels: Seq[C], index: Int): Unit

  private[rspace] def removeAll(txn: T, channels: Seq[C]): Unit

  // compare to store.joinMap.addBinding
  private[rspace] def addJoin(txn: T, channel: C, channels: Seq[C]): Unit

  // compare to store.joinMap.get(c).toList.flatten
  private[rspace] def getJoin(txn: T, channel: C): Seq[Seq[C]]

  // compare to store.joinMap.removeBinding
  private[rspace] def removeJoin(txn: T, channel: C, channels: Seq[C]): Unit

  // compare to store.joinMap.remove
  private[rspace] def removeAllJoins(txn: T, channel: C): Unit

  private[rspace] def collectGarbage(txn: T,
                                     channelsHash: H,
                                     dataCollected: Boolean = false,
                                     waitingContinuationsCollected: Boolean = false,
                                     joinsCollected: Boolean = false): Unit

  def toMap: Map[Seq[C], Row[P, A, K]]

  def close(): Unit
}
