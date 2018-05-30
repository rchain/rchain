package coop.rchain.rspace.test

import java.nio.charset.StandardCharsets

import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.rspace.examples._
import coop.rchain.rspace.history.{Blake2b256Hash, Trie}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.util.dropIndex
import coop.rchain.rspace.{IStore, ITestableStore, Serialize, StoreSize}
import javax.xml.bind.DatatypeConverter.printHexBinary

import scala.collection.immutable.Seq
import scala.collection.mutable

class InMemoryStore[C, P, A, K <: Serializable] private (
    _keys: mutable.HashMap[String, Seq[C]],
    _waitingContinuations: mutable.HashMap[String, Seq[WaitingContinuation[P, K]]],
    _data: mutable.HashMap[String, Seq[Datum[A]]],
    _joinMap: mutable.HashMap[C, Seq[Seq[C]]],
)(implicit sc: Serialize[C])
    extends IStore[C, P, A, K]
    with ITestableStore[C, P] {

  private[rspace] type H = String

  private[rspace] type T = Unit

  private[rspace] def hashChannels(cs: Seq[C])(implicit sc: Serialize[C]): H =
    printHexBinary(InMemoryStore.hashBytes(cs.flatMap(sc.encode).toArray))

  private[rspace] def putCs(txn: T, channels: Seq[C]): Unit =
    _keys.update(hashChannels(channels), channels)

  private[rspace] def getChannels(txn: T, s: H) =
    _keys.getOrElse(s, Seq.empty[C])

  private[rspace] def createTxnRead(): Unit = ()

  private[rspace] def createTxnWrite(): Unit = ()

  private[rspace] def withTxn[R](txn: T)(f: T => R): R =
    f(txn)

  private[rspace] def collectGarbage(txn: T,
                                     channelsHash: H,
                                     dataCollected: Boolean = false,
                                     waitingContinuationsCollected: Boolean = false,
                                     joinsCollected: Boolean = false): Unit =
    collectGarbage(channelsHash)

  private[this] def collectGarbage(key: H): Unit = {
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

    val cs    = _keys.getOrElse(key, Seq.empty[C])
    val joins = cs.size == 1 && _joinMap.contains(cs.head)

    if (!as && !psks && !joins) {
      _keys.remove(key)
    }
  }

  private[rspace] def putDatum(txn: T, channels: Seq[C], datum: Datum[A]): Unit = {
    val key = hashChannels(channels)
    putCs(txn, channels)
    val datums = _data.getOrElseUpdate(key, Seq.empty[Datum[A]])
    _data.update(key, datum +: datums)
  }

  private[rspace] def putWaitingContinuation(txn: T,
                                             channels: Seq[C],
                                             continuation: WaitingContinuation[P, K]): Unit = {
    val key = hashChannels(channels)
    putCs(txn, channels)
    val waitingContinuations =
      _waitingContinuations.getOrElseUpdate(key, Seq.empty[WaitingContinuation[P, K]])
    _waitingContinuations.update(key, continuation +: waitingContinuations)
  }

  private[rspace] def getData(txn: T, channels: Seq[C]): Seq[Datum[A]] =
    _data.getOrElse(hashChannels(channels), Seq.empty[Datum[A]])

  private[rspace] def getWaitingContinuation(txn: T, curr: Seq[C]): Seq[WaitingContinuation[P, K]] =
    _waitingContinuations
      .getOrElse(hashChannels(curr), Seq.empty[WaitingContinuation[P, K]])
      .map { (wk: WaitingContinuation[P, K]) =>
        wk.copy(continuation = InMemoryStore.roundTrip(wk.continuation))
      }

  private[rspace] def removeDatum(txn: T, channel: C, index: Int): Unit =
    removeDatum(txn, Seq(channel), index)

  private[rspace] def removeDatum(txn: T, channels: Seq[C], index: Int): Unit = {
    val key = hashChannels(channels)
    for (as <- _data.get(key)) {
      _data.update(key, dropIndex(as, index))
    }
    collectGarbage(key)
  }

  private[rspace] def removeWaitingContinuation(txn: T, channels: Seq[C], index: Int): Unit = {
    val key = hashChannels(channels)
    for (psks <- _waitingContinuations.get(key)) {
      _waitingContinuations.update(key, dropIndex(psks, index))
    }
    collectGarbage(key)
  }

  private[rspace] def removeAll(txn: Unit, channels: Seq[C]): Unit = {
    val key = hashChannels(channels)
    _data.put(key, Seq.empty)
    _waitingContinuations.put(key, Seq.empty)
    for (c <- channels) removeJoin(txn, c, channels)
  }

  private[rspace] def addJoin(txn: T, c: C, cs: Seq[C]): Unit = {
    val existing: Seq[Seq[C]] = _joinMap.remove(c).getOrElse(Seq.empty)
    if (!existing.exists(_.equals(cs)))
      _joinMap.put(c, cs +: existing)
    else
      _joinMap.put(c, existing)
  }

  private[rspace] def getJoin(txn: T, c: C): Seq[Seq[C]] =
    _joinMap.getOrElse(c, Seq.empty[Seq[C]])

  private[rspace] def removeJoin(txn: T, c: C, cs: Seq[C]): Unit = {
    val joinKey = hashChannels(Seq(c))
    val csKey   = hashChannels(cs)
    if (_waitingContinuations.get(csKey).forall(_.isEmpty)) {
      val existing: Seq[Seq[C]] = _joinMap.remove(c).getOrElse(Seq.empty)
      val filtered              = existing.filter(!_.equals(cs))
      if (filtered.nonEmpty)
        _joinMap.put(c, filtered)
    }
    collectGarbage(joinKey)
  }

  private[rspace] def removeAllJoins(txn: T, c: C): Unit = {
    _joinMap.remove(c)
    collectGarbage(hashChannels(Seq(c)))
  }

  def close(): Unit = ()

  def getPatterns(txn: T, channels: Seq[C]): Seq[Seq[P]] =
    _waitingContinuations.getOrElse(hashChannels(channels), Nil).map(_.patterns)

  def clear(): Unit = {
    _keys.clear()
    _waitingContinuations.clear()
    _data.clear()
    _joinMap.clear()
  }

  def getStoreSize: StoreSize =
    StoreSize(0, (_keys.size + _waitingContinuations.size + _data.size + _joinMap.size).toLong)

  def isEmpty: Boolean =
    _waitingContinuations.isEmpty && _data.isEmpty && _keys.isEmpty && _joinMap.isEmpty

  def toMap: Map[Seq[C], Row[P, A, K]] =
    _keys.map {
      case (hash, cs) =>
        val data = _data.getOrElse(hash, Seq.empty[Datum[A]])
        val wks  = _waitingContinuations.getOrElse(hash, Seq.empty[WaitingContinuation[P, K]])
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
    Blake2b256.hash(bs)

  def hashString(s: String): Array[Byte] =
    hashBytes(s.getBytes(StandardCharsets.UTF_8))

  def create[C, P, A, K <: Serializable](implicit sc: Serialize[C]): InMemoryStore[C, P, A, K] =
    new InMemoryStore[C, P, A, K](
      _keys = mutable.HashMap.empty[String, Seq[C]],
      _waitingContinuations = mutable.HashMap.empty[String, Seq[WaitingContinuation[P, K]]],
      _data = mutable.HashMap.empty[String, Seq[Datum[A]]],
      _joinMap = mutable.HashMap.empty[C, Seq[Seq[C]]]
    )
}
