package coop.rchain.rspace.test

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import coop.rchain.rspace.examples._
import coop.rchain.rspace.internal._
import coop.rchain.rspace.util.dropIndex
import coop.rchain.rspace.{IStore, ITestableStore, Serialize}
import javax.xml.bind.DatatypeConverter.printHexBinary

import scala.collection.mutable

class InMemoryStore[C, P, A, K <: Serializable] private (
    _keys: mutable.HashMap[String, List[C]],
    _waitingContinuations: mutable.HashMap[String, List[WaitingContinuation[P, K]]],
    _data: mutable.HashMap[String, List[Datum[A]]],
    _joinMap: mutable.MultiMap[C, String]
)(implicit sc: Serialize[C])
    extends IStore[C, P, A, K]
    with ITestableStore[C, P] {

  private[rspace] type H = String

  private[rspace] type T = Unit

  private[rspace] def hashCs(cs: List[C])(implicit sc: Serialize[C]): H =
    printHexBinary(InMemoryStore.hashBytes(cs.flatMap(sc.encode).toArray))

  private[rspace] def putCs(txn: T, channels: List[C]): Unit =
    _keys.update(hashCs(channels), channels)

  private[rspace] def getKey(txn: T, s: H) =
    _keys.getOrElse(s, List.empty[C])

  private[rspace] def createTxnRead(): Unit = ()

  private[rspace] def createTxnWrite(): Unit = ()

  private[rspace] def withTxn[R](txn: T)(f: T => R): R =
    f(txn)

  def collectGarbage(key: H): Unit = {
    val as = _data.get(key).exists(_.nonEmpty)
    if (!as) {
      //we still may have empty list, remove it as well
      _data.remove(key)
    }

    val psks = _waitingContinuations.get(key).exists(_.nonEmpty)
    if (!psks) {
      //we still may have empty list, remove it as well
      _waitingContinuations.remove(key)
    }

    val cs    = _keys.getOrElse(key, List.empty[C])
    val joins = cs.size == 1 && _joinMap.contains(cs.head)

    if (!as && !psks && !joins) {
      _keys.remove(key)
    }
  }

  private[rspace] def putA(txn: T, channels: List[C], datum: Datum[A]): Unit = {
    val key = hashCs(channels)
    putCs(txn, channels)
    val datums = _data.getOrElseUpdate(key, List.empty[Datum[A]])
    _data.update(key, datum +: datums)
  }

  private[rspace] def putK(txn: T,
                           channels: List[C],
                           continuation: WaitingContinuation[P, K]): Unit = {
    val key = hashCs(channels)
    putCs(txn, channels)
    val waitingContinuations =
      _waitingContinuations.getOrElseUpdate(key, List.empty[WaitingContinuation[P, K]])
    _waitingContinuations.update(key, waitingContinuations :+ continuation)
  }

  private[rspace] def getAs(txn: T, channels: List[C]): List[Datum[A]] =
    _data.getOrElse(hashCs(channels), List.empty[Datum[A]])

  private[rspace] def getPsK(txn: T, curr: List[C]): List[WaitingContinuation[P, K]] =
    _waitingContinuations
      .getOrElse(hashCs(curr), List.empty[WaitingContinuation[P, K]])
      .map { (wk: WaitingContinuation[P, K]) =>
        wk.copy(continuation = InMemoryStore.roundTrip(wk.continuation))
      }

  private[rspace] def removeA(txn: T, channel: C, index: Int): Unit =
    removeA(txn, List(channel), index)

  private[rspace] def removeA(txn: T, channels: List[C], index: Int): Unit = {
    val key = hashCs(channels)
    for (as <- _data.get(key)) {
      _data.update(key, dropIndex(as, index))
    }
    collectGarbage(key)
  }

  private[rspace] def removePsK(txn: T, channels: List[C], index: Int): Unit = {
    val key = hashCs(channels)
    for (psks <- _waitingContinuations.get(key)) {
      _waitingContinuations.update(key, dropIndex(psks, index))
    }
    collectGarbage(key)
  }

  private[rspace] def removeAll(txn: Unit, channels: List[C]): Unit = {
    val key = hashCs(channels)
    _data.put(key, List.empty)
    _waitingContinuations.put(key, List.empty)
    for (c <- channels) removeJoin(txn, c, channels)
  }

  private[rspace] def addJoin(txn: T, c: C, cs: List[C]): Unit =
    _joinMap.addBinding(c, hashCs(cs))

  private[rspace] def getJoin(txn: T, c: C): List[List[C]] =
    _joinMap.getOrElse(c, Set.empty[String]).toList.map(getKey(txn, _))

  private[rspace] def removeJoin(txn: T, c: C, cs: List[C]): Unit = {
    val joinKey = hashCs(List(c))
    val csKey   = hashCs(cs)
    if (_waitingContinuations.get(csKey).forall(_.isEmpty)) {
      _joinMap.removeBinding(c, csKey)
    }
    collectGarbage(joinKey)
  }

  private[rspace] def removeAllJoins(txn: T, c: C): Unit = {
    _joinMap.remove(c)
    collectGarbage(hashCs(List(c)))
  }

  def close(): Unit = ()

  def getPs(txn: T, channels: List[C]): List[List[P]] =
    _waitingContinuations.getOrElse(hashCs(channels), Nil).map(_.patterns)

  def clear(): Unit = {
    _keys.clear()
    _waitingContinuations.clear()
    _data.clear()
    _joinMap.clear()
  }

  def isEmpty: Boolean =
    _waitingContinuations.isEmpty && _data.isEmpty && _keys.isEmpty && _joinMap.isEmpty

  def toMap: Map[List[C], Row[P, A, K]] =
    _keys.map {
      case (hash, cs) =>
        val data = _data.getOrElse(hash, List.empty[Datum[A]])
        val wks  = _waitingContinuations.getOrElse(hash, List.empty[WaitingContinuation[P, K]])
        (cs, Row(data, wks))
    }.toMap
}

object InMemoryStore {

  /* UGLY HACK FOR TESTING */
  def roundTrip[A <: Serializable](a: A): A = {
    val ser = makeSerializeFromSerializable[A]
    ser.decode(ser.encode(a)).fold(throw _, identity)
  }

  def hashBytes(bs: Array[Byte]): Array[Byte] =
    MessageDigest.getInstance("SHA-256").digest(bs)

  def hashString(s: String): Array[Byte] =
    hashBytes(s.getBytes(StandardCharsets.UTF_8))

  def create[C, P, A, K <: Serializable](implicit sc: Serialize[C]): InMemoryStore[C, P, A, K] =
    new InMemoryStore[C, P, A, K](
      _keys = mutable.HashMap.empty[String, List[C]],
      _waitingContinuations = mutable.HashMap.empty[String, List[WaitingContinuation[P, K]]],
      _data = mutable.HashMap.empty[String, List[Datum[A]]],
      _joinMap = new mutable.HashMap[C, mutable.Set[String]] with mutable.MultiMap[C, String]
    )
}
