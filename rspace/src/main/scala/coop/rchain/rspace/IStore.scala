package coop.rchain.rspace

import coop.rchain.rspace.internal._

import scala.collection.mutable

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

  private[rspace] def hashCs(channels: List[C])(implicit sc: Serialize[C]): H

  private[rspace] def getKey(txn: T, hash: H): List[C]

  private[rspace] def removeA(txn: T, channels: List[C], index: Int): Unit

  private[rspace] def createTxnRead(): T

  private[rspace] def createTxnWrite(): T

  private[rspace] def withTxn[R](txn: T)(f: T => R): R

  private[rspace] def putA(txn: T, channels: List[C], datum: Datum[A]): Unit

  private[rspace] def putK(txn: T, channels: List[C], continuation: WaitingContinuation[P, K]): Unit

  private[rspace] def getAs(txn: T, channels: List[C]): List[Datum[A]]

  private[rspace] def getPsK(txn: T, curr: List[C]): List[WaitingContinuation[P, K]]

  private[rspace] def removeA(txn: T, channel: C, index: Int): Unit

  private[rspace] def removePsK(txn: T, channels: List[C], index: Int): Unit

  // compare to store.joinMap.addBinding
  private[rspace] def addJoin(txn: T, c: C, cs: List[C]): Unit

  // compare to store.joinMap.get(c).toList.flatten
  private[rspace] def getJoin(txn: T, c: C): List[List[C]]

  // compare to store.joinMap.removeBinding
  private[rspace] def removeJoin(txn: T, c: C, cs: List[C]): Unit

  // compare to store.joinMap.remove
  private[rspace] def removeAllJoins(txn: T, c: C): Unit

  def toMap: Map[List[C], Row[P, A, K]]

  def close(): Unit
}
