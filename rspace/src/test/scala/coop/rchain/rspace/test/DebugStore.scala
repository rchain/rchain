package coop.rchain.rspace.test

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import coop.rchain.rspace.examples._
import coop.rchain.rspace.internal._
import coop.rchain.rspace.util.dropIndex
import coop.rchain.rspace.{IStore, ITestableStore, Serialize}
import javax.xml.bind.DatatypeConverter.printHexBinary

import scala.collection.immutable.Seq
import scala.collection.mutable

class DebugStore[C, P, A, K <: Serializable] private (
    _keys: mutable.HashMap[String, Seq[C]],
    _waitingContinuations: mutable.HashMap[String, Seq[WaitingContinuation[P, K]]],
    _data: mutable.HashMap[String, Seq[Datum[A]]],
    _joinMap: mutable.MultiMap[C, String]
)(implicit sc: Serialize[C])
    extends IStore[C, P, A, K]
    with ITestableStore[C, P] {

  private[rspace] type H = String

  private[rspace] type T = Unit

  private[rspace] def hashChannels(channels: Seq[C])(implicit sc: Serialize[C]): H =
    printHexBinary(DebugStore.hashBytes(channels.flatMap(sc.encode).toArray))

  private[rspace] def putChannels(txn: T, channels: Seq[C]): Unit =
    _keys.update(hashChannels(channels), channels)

  private[rspace] def getChannels(txn: T, channelsHash: H) =
    _keys.getOrElse(channelsHash, Seq.empty[C])

  private[rspace] def createTxnRead(): Unit = ()

  private[rspace] def createTxnWrite(): Unit = ()

  private[rspace] def withTxn[R](txn: T)(f: T => R): R =
    f(txn)

  def collectGarbage(channelsHash: H): Unit = {
    val datum = _data.get(channelsHash).exists(_.nonEmpty)
    if (!datum) {
      //we still may have empty list, remove it as well
      _data.remove(channelsHash)
    }

    val waitingContinuation = _waitingContinuations.get(channelsHash).exists(_.nonEmpty)
    if (!waitingContinuation) {
      //we still may have empty list, remove it as well
      _waitingContinuations.remove(channelsHash)
    }

    val channels = _keys.getOrElse(channelsHash, Seq.empty[C])
    val joins    = channels.size == 1 && _joinMap.contains(channels.head)

    if (!datum && !waitingContinuation && !joins) {
      _keys.remove(channelsHash)
    }
  }

  private[rspace] def putDatum(txn: T, channels: Seq[C], datum: Datum[A]): Unit = {
    val channelsHash = hashChannels(channels)
    putChannels(txn, channels)
    val oldDatums = _data.getOrElseUpdate(channelsHash, Seq.empty[Datum[A]])
    _data.update(channelsHash, datum +: oldDatums)
  }

  private[rspace] def putWaitingContinuation(txn: T,
                                             channels: Seq[C],
                                             continuation: WaitingContinuation[P, K]): Unit = {
    val channelsHash = hashChannels(channels)
    putChannels(txn, channels)
    val waitingContinuations =
      _waitingContinuations.getOrElseUpdate(channelsHash, Seq.empty[WaitingContinuation[P, K]])
    _waitingContinuations.update(channelsHash, waitingContinuations :+ continuation)
  }

  private[rspace] def getData(txn: T, channels: Seq[C]): Seq[Datum[A]] =
    _data.getOrElse(hashChannels(channels), Seq.empty[Datum[A]])

  private[rspace] def getWaitingContinuation(txn: T,
                                             channels: Seq[C]): Seq[WaitingContinuation[P, K]] =
    _waitingContinuations
      .getOrElse(hashChannels(channels), Seq.empty[WaitingContinuation[P, K]])
      .map(waitingContinuation =>
        waitingContinuation.copy(
          continuation = DebugStore.roundTrip(waitingContinuation.continuation)))

  private[rspace] def removeDatum(txn: T, channel: C, index: Int): Unit =
    removeDatum(txn, Seq(channel), index)

  private[rspace] def removeDatum(txn: T, channels: Seq[C], index: Int): Unit = {
    val channelsHash = hashChannels(channels)
    for (datum <- _data.get(channelsHash)) {
      _data.update(channelsHash, dropIndex(datum, index))
    }
    collectGarbage(channelsHash)
  }

  private[rspace] def removeWaitingContinuation(txn: T, channels: Seq[C], index: Int): Unit = {
    val channelsHash = hashChannels(channels)
    for (waitingContinuation <- _waitingContinuations.get(channelsHash)) {
      _waitingContinuations.update(channelsHash, dropIndex(waitingContinuation, index))
    }
    collectGarbage(channelsHash)
  }

  private[rspace] def removeAll(txn: Unit, channels: Seq[C]): Unit = {
    val channelsHash = hashChannels(channels)
    _data.put(channelsHash, Seq.empty)
    _waitingContinuations.put(channelsHash, Seq.empty)
    for (channel <- channels) removeJoin(txn, channel, channels)
  }

  private[rspace] def addJoin(txn: T, channel: C, channels: Seq[C]): Unit =
    _joinMap.addBinding(channel, hashChannels(channels))

  private[rspace] def getJoin(txn: T, channel: C): Seq[Seq[C]] =
    _joinMap.getOrElse(channel, Set.empty[String]).toList.map(getChannels(txn, _))

  private[rspace] def removeJoin(txn: T, channel: C, channels: Seq[C]): Unit = {
    val joinKey      = hashChannels(Seq(channel))
    val channelsHash = hashChannels(channels)
    if (_waitingContinuations.get(channelsHash).forall(_.isEmpty)) {
      _joinMap.removeBinding(channel, channelsHash)
    }
    collectGarbage(joinKey)
  }

  private[rspace] def removeAllJoins(txn: T, channel: C): Unit = {
    _joinMap.remove(channel)
    collectGarbage(hashChannels(Seq(channel)))
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

  def isEmpty: Boolean =
    _waitingContinuations.isEmpty && _data.isEmpty && _keys.isEmpty && _joinMap.isEmpty

  def toMap: Map[Seq[C], Row[P, A, K]] =
    _keys.map {
      case (channelKey, channels) =>
        val data = _data.getOrElse(channelKey, Seq.empty[Datum[A]])
        val waitingContinuations =
          _waitingContinuations.getOrElse(channelKey, Seq.empty[WaitingContinuation[P, K]])
        (channels, Row(data, waitingContinuations))
    }.toMap
}

object DebugStore {
  /* UGLY HACK FOR TESTING */
  def roundTrip[A <: Serializable](a: A): A = {
    val ser = makeSerializeFromSerializable[A]
    ser.decode(ser.encode(a)).fold(throw _, identity)
  }

  def hashBytes(bs: Array[Byte]): Array[Byte] =
    MessageDigest.getInstance("SHA-256").digest(bs)

  def hashString(s: String): Array[Byte] =
    hashBytes(s.getBytes(StandardCharsets.UTF_8))

  def create[C, P, A, K <: Serializable](implicit sc: Serialize[C]): DebugStore[C, P, A, K] =
    new DebugStore[C, P, A, K](
      _keys = mutable.HashMap.empty[String, Seq[C]],
      _waitingContinuations = mutable.HashMap.empty[String, Seq[WaitingContinuation[P, K]]],
      _data = mutable.HashMap.empty[String, Seq[Datum[A]]],
      _joinMap = new mutable.HashMap[C, mutable.Set[String]] with mutable.MultiMap[C, String]
    )
}
