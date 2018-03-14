package coop.rchain.storage.test

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import coop.rchain.models.Serialize
import coop.rchain.storage.IStore
import coop.rchain.storage.util.dropIndex
import javax.xml.bind.DatatypeConverter.printHexBinary

import scala.collection.mutable

class InMemoryStore[C, P, A, K] private (
    _keys: mutable.HashMap[String, List[C]],
    _psks: mutable.HashMap[String, List[(List[P], K)]],
    _as: mutable.HashMap[String, List[A]],
    _joinMap: mutable.MultiMap[C, String]
)(implicit sc: Serialize[C])
    extends IStore[C, P, A, K] {

  type H = String

  private[storage] def hashCs(cs: List[C])(implicit sc: Serialize[C]): H =
    printHexBinary(InMemoryStore.hashBytes(cs.flatMap(sc.encode).toArray))

  private[storage] def putCs(txn: T, channels: List[C]): Unit =
    _keys.update(hashCs(channels), channels)

  private[storage] def getKey(txn: T, s: H) =
    _keys.getOrElse(s, List.empty[C])

  type T = Unit

  def createTxnRead(): Unit = ()

  def createTxnWrite(): Unit = ()

  def withTxn[R](txn: T)(f: T => R): R =
    f(txn)

  def putA(txn: T, channels: List[C], a: A): Unit = {
    val key = hashCs(channels)
    putCs(txn, channels)
    val as = _as.getOrElseUpdate(key, List.empty[A])
    _as.update(key, scala.util.Random.shuffle(a +: as))
  }

  def putK(txn: T, channels: List[C], patterns: List[P], k: K): Unit = {
    val key = hashCs(channels)
    putCs(txn, channels)
    val ps = _psks.getOrElseUpdate(key, List.empty[(List[P], K)])
    _psks.update(key, ps :+ (patterns, k))
  }

  def getPs(txn: T, channels: List[C]): List[List[P]] =
    _psks.getOrElse(hashCs(channels), Nil).map(_._1)

  def getAs(txn: T, channels: List[C]): List[A] =
    _as.getOrElse(hashCs(channels), Nil)

  def getPsK(txn: T, curr: List[C]): List[(List[P], K)] =
    _psks.getOrElse(hashCs(curr), List.empty[(List[P], K)])

  def removeA(txn: T, channel: C, index: Int): Unit = {
    val key = hashCs(List(channel))
    for (as <- _as.get(key)) {
      _as.update(key, dropIndex(as, index))
    }
  }

  def removePsK(txn: T, channels: List[C], index: Int): Unit = {
    val key = hashCs(channels)
    for (psks <- _psks.get(key)) {
      _psks.update(key, dropIndex(psks, index))
    }
  }

  def addJoin(txn: T, c: C, cs: List[C]): Unit =
    _joinMap.addBinding(c, hashCs(cs))

  def getJoin(txn: T, c: C): List[List[C]] =
    _joinMap.getOrElse(c, Set.empty[String]).toList.map(getKey(txn, _))

  def removeJoin(txn: T, c: C, cs: List[C]): Unit = {
    val key = hashCs(cs)
    for (psks <- _psks.get(key) if psks.isEmpty) {
      _joinMap.removeBinding(c, key)
    }
  }

  def removeAllJoins(txn: T, c: C): Unit =
    _joinMap.remove(c)

  def close(): Unit = ()
}

object InMemoryStore {

  def hashBytes(bs: Array[Byte]): Array[Byte] =
    MessageDigest.getInstance("SHA-256").digest(bs)

  def hashString(s: String): Array[Byte] =
    hashBytes(s.getBytes(StandardCharsets.UTF_8))

  def create[C, P, A, K](implicit sc: Serialize[C]): InMemoryStore[C, P, A, K] =
    new InMemoryStore[C, P, A, K](
      _keys = mutable.HashMap.empty[String, List[C]],
      _psks = mutable.HashMap.empty[String, List[(List[P], K)]],
      _as = mutable.HashMap.empty[String, List[A]],
      _joinMap = new mutable.HashMap[C, mutable.Set[String]] with mutable.MultiMap[C, String]
    )
}
