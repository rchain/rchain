package coop.rchain.storage

import coop.rchain.storage.internal._

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
  private[storage] type H

  /**
    * The type of transactions
    */
  private[storage] type T

  private[storage] def hashCs(channels: List[C])(implicit sc: Serialize[C]): H

  private[storage] def getKey(txn: T, hash: H): List[C]

  private[storage] def removeA(txn: T, channels: List[C], index: Int): Unit

  private[storage] def createTxnRead(): T

  private[storage] def createTxnWrite(): T

  private[storage] def withTxn[R](txn: T)(f: T => R): R

  private[storage] def putA(txn: T, channels: List[C], datum: Datum[A]): Unit

  private[storage] def putK(txn: T,
                            channels: List[C],
                            continuation: WaitingContinuation[P, K]): Unit

  private[storage] def getAs(txn: T, channels: List[C]): List[Datum[A]]

  private[storage] def getPsK(txn: T, curr: List[C]): List[WaitingContinuation[P, K]]

  private[storage] def removeA(txn: T, channel: C, index: Int): Unit

  private[storage] def removePsK(txn: T, channels: List[C], index: Int): Unit

  // compare to store.joinMap.addBinding
  private[storage] def addJoin(txn: T, c: C, cs: List[C]): Unit

  // compare to store.joinMap.get(c).toList.flatten
  private[storage] def getJoin(txn: T, c: C): List[List[C]]

  // compare to store.joinMap.removeBinding
  private[storage] def removeJoin(txn: T, c: C, cs: List[C]): Unit

  // compare to store.joinMap.remove
  private[storage] def removeAllJoins(txn: T, c: C): Unit

  def toMap: Map[List[C], Row[P, A, K]]

  def close(): Unit
}
