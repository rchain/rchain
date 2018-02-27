package coop.rchain.storage

import java.nio.ByteBuffer

import org.lmdbjava.Txn

trait IStore[C, P, A, K] {

  private[storage] def hashC(cs: List[C]): String

  private[storage] def putCs(txn: Txn[ByteBuffer], channels: List[C]): Unit

  private[storage] def getKey(txn: Txn[ByteBuffer], s: String): List[C]

  def getTxnRead(): Txn[ByteBuffer]

  def getTxnWrite(): Txn[ByteBuffer]

  def putA(txn: Txn[ByteBuffer], channels: List[C], a: A): Unit

  def putK(txn: Txn[ByteBuffer], channels: List[C], patterns: List[P], k: K): Unit

  def getPs(txn: Txn[ByteBuffer], channels: List[C]): List[P]

  def getAs(txn: Txn[ByteBuffer], channels: List[C]): List[A]

  def getK(txn: Txn[ByteBuffer], curr: List[C]): Option[(List[P], K)]

  def removeA(txn: Txn[ByteBuffer], channels: List[C], index: Int): Unit

  def removeK(txn: Txn[ByteBuffer], channels: List[C], index: Int): Unit

  // compare to joinMap.addBinding
  def addJoin(c: C, cs: List[C]): Unit

  // compare to joinMap.removeBinding
  def removeJoin(c: C, cs: List[C]): Unit

  // compare to joinMap.remove
  def removeAllJoins(c: C): Unit
}
