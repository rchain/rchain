package coop.rchain.storage

import coop.rchain.models.Serialize

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
  type H

  private[storage] def hashCs(channels: List[C])(implicit sc: Serialize[C]): H

  private[storage] def putCs(txn: T, channels: List[C]): Unit

  private[storage] def getKey(txn: T, hash: H): List[C]

  /**
    * The type of transactions
    */
  type T

  def createTxnRead(): T

  def createTxnWrite(): T

  def withTxn[R](txn: T)(f: T => R): R

  def putA(txn: T, channels: List[C], a: A): Unit

  def putK(txn: T, channels: List[C], patterns: List[P], k: K): Unit

  def getPs(txn: T, channels: List[C]): List[List[P]]

  def getAs(txn: T, channels: List[C]): List[A]

  def getPsK(txn: T, curr: List[C]): List[(List[P], K)]

  def removeA(txn: T, channel: C, index: Int): Unit

  def removePsK(txn: T, channels: List[C], index: Int): Unit

  // compare to store.joinMap.addBinding
  def addJoin(txn: T, c: C, cs: List[C]): Unit

  // compare to store.joinMap.get(c).toList.flatten
  def getJoin(txn: T, c: C): List[List[C]]

  // compare to store.joinMap.removeBinding
  def removeJoin(txn: T, c: C, cs: List[C]): Unit

  // compare to store.joinMap.remove
  def removeAllJoins(txn: T, c: C): Unit

  def close(): Unit
}
