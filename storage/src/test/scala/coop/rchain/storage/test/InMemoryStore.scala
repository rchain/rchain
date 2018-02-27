package coop.rchain.storage.test

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import coop.rchain.models.Serialize
import coop.rchain.storage.IStore
import javax.xml.bind.DatatypeConverter.printHexBinary

import scala.collection.mutable

class InMemoryStore[C, P, A, K] private (
    _keys: mutable.HashMap[String, List[C]],
    _ps: mutable.HashMap[String, List[P]],
    _as: mutable.HashMap[String, List[A]],
    _k: mutable.HashMap[String, K],
    val joinMap: mutable.MultiMap[C, String]
)(implicit sc: Serialize[C])
    extends IStore[C, P, A, K, DummyTransaction] {

  type H = String

  private[storage] def hashC(cs: List[C])(implicit sc: Serialize[C]): H =
    printHexBinary(InMemoryStore.hashBytes(cs.flatMap(sc.encode).toArray))

  private[storage] def putCs(txn: DummyTransaction, channels: List[C]): Unit =
    _keys.update(hashC(channels), channels)

  private[storage] def getKey(txn: DummyTransaction, s: String) =
    _keys.get(s).toList.flatten

  def createTxnRead(): DummyTransaction = DummyTransaction()

  def createTxnWrite(): DummyTransaction = DummyTransaction()

  def putA(txn: DummyTransaction, channels: List[C], a: A): Unit = {
    val key = hashC(channels)
    putCs(txn, channels)
    val as = _as.getOrElseUpdate(key, List.empty[A])
    _as.update(key, a +: as)
  }

  def putK(txn: DummyTransaction, channels: List[C], patterns: List[P], k: K): Unit = {
    val key = hashC(channels)
    putCs(txn, channels)
    val ps = _ps.getOrElseUpdate(key, List.empty[P])
    _ps.update(key, patterns ++ ps)
    _k.update(key, k)
  }

  def getPs(txn: DummyTransaction, channels: List[C]): List[P] =
    _ps.getOrElse(hashC(channels), Nil)

  def getAs(txn: DummyTransaction, channels: List[C]): List[A] =
    _as.getOrElse(hashC(channels), Nil)

  def getK(txn: DummyTransaction, curr: List[C]): Option[(List[P], K)] = {
    val key = hashC(curr)
    for {
      ps <- _ps.get(key)
      k  <- _k.get(key)
    } yield (ps, k)
  }

  def removeA(txn: DummyTransaction, channels: List[C], index: Int): Unit = {
    val key = hashC(channels)
    for (as <- _as.get(key)) {
      _as.update(key, dropIndex(as, index))
    }
  }

  def removeK(txn: DummyTransaction, channels: List[C], index: Int): Unit = {
    val key = hashC(channels)
    for (ps <- _ps.get(key)) {
      _ps.update(key, dropIndex(ps, index))
      _k.remove(key)
    }
  }

  def addJoin(txn: DummyTransaction, c: C, cs: List[C]): Unit =
    joinMap.addBinding(c, hashC(cs))

  def getJoin(txn: DummyTransaction, c: C): List[List[C]] =
    joinMap.get(c).toList.flatten.map(getKey(txn, _))

  def removeJoin(txn: DummyTransaction, c: C, cs: List[C]): Unit =
    joinMap.removeBinding(c, hashC(cs))

  def removeAllJoins(txn: DummyTransaction, c: C): Unit =
    joinMap.remove(c)

  def close(): Unit = ()
}

object InMemoryStore {

  def hashBytes(bs: Array[Byte]): Array[Byte] =
    MessageDigest.getInstance("SHA-256").digest(bs)

  def hashString(s: String): Array[Byte] =
    hashBytes(s.getBytes(StandardCharsets.UTF_8))

  def create[C, P, A, K]()(implicit sc: Serialize[C]): InMemoryStore[C, P, A, K] =
    new InMemoryStore[C, P, A, K](
      _keys = mutable.HashMap.empty[String, List[C]],
      _ps = mutable.HashMap.empty[String, List[P]],
      _as = mutable.HashMap.empty[String, List[A]],
      _k = mutable.HashMap.empty[String, K],
      joinMap = new mutable.HashMap[C, mutable.Set[String]] with mutable.MultiMap[C, String]
    )
}
