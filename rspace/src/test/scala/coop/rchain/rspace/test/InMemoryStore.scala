package coop.rchain.rspace.test

import java.nio.charset.StandardCharsets

import cats.implicits._
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.rspace.examples._
import coop.rchain.rspace.history.{Blake2b256Hash, Trie}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.util.dropIndex
import coop.rchain.rspace.{IStore, ITestableStore, Serialize, StoreCounters, StoreEventsCounter}
import javax.xml.bind.DatatypeConverter.printHexBinary
import scodec.Codec
import scodec.bits.BitVector
import coop.rchain.shared.AttemptOps._

import scala.collection.immutable.Seq
import scala.collection.mutable

class InMemoryStore[C, P, A, K] private (
    _dbGNATs: mutable.Map[Blake2b256Hash, GNAT[C, P, A, K]],
    _dbJoins: mutable.HashMap[C, Seq[Seq[C]]]
)(implicit sc: Serialize[C], sk: Serialize[K])
    extends IStore[C, P, A, K]
    with ITestableStore[C, P] {

  private implicit val codecC: Codec[C] = sc.toCodec
  private implicit val codecK: Codec[K] = sk.toCodec

  val eventsCounter: StoreEventsCounter = new StoreEventsCounter()

  private[rspace] type H = Blake2b256Hash

  private[rspace] type T = Unit

  private[rspace] override def hashChannels(channels: Seq[C]): H =
    Codec[Seq[C]]
      .encode(channels)
      .map((bitVec: BitVector) => Blake2b256Hash.create(bitVec.toByteArray))
      .get

  private[rspace] override def createTxnRead(): Unit = ()

  private[rspace] override def createTxnWrite(): Unit = ()

  private[rspace] override def withTxn[R](txn: T)(f: T => R): R =
    f(txn)

  private[rspace] override def getChannels(txn: T, s: H) =
    _dbGNATs.get(s).map(_.channels).getOrElse(Seq.empty)

  private[rspace] override def getData(txn: T, channels: Seq[C]): Seq[Datum[A]] =
    _dbGNATs.get(hashChannels(channels)).map(_.data).getOrElse(Seq.empty[Datum[A]])

  private[this] def getWK(txn: T, curr: Seq[C]): Seq[WaitingContinuation[P, K]] =
    _dbGNATs
      .get(hashChannels(curr))
      .map(_.wks)
      .getOrElse(Seq.empty[WaitingContinuation[P, K]])

  private[rspace] override def getWaitingContinuation(
      txn: T,
      curr: Seq[C]): Seq[WaitingContinuation[P, K]] =
    getWK(txn, curr)
      .map { (wk: WaitingContinuation[P, K]) =>
        wk.copy(continuation = InMemoryStore.roundTrip(wk.continuation))
      }

  private[rspace] override def getJoin(txn: T, c: C): Seq[Seq[C]] =
    _dbJoins.getOrElse(c, Seq.empty[Seq[C]])

  override def getPatterns(txn: T, channels: Seq[C]): Seq[Seq[P]] =
    getWK(txn, channels).map(_.patterns)

  private[this] def withGNAT(key: H)(
      f: (H, Option[GNAT[C, P, A, K]]) => Option[GNAT[C, P, A, K]]): Unit = {
    val gnatOpt = _dbGNATs.get(key)
    val rOpt    = f(key, gnatOpt)
    rOpt match {
      case Some(gnat) if isGNATOrphaned(gnat) => _dbGNATs -= key
      case Some(gnat)                         => _dbGNATs += key -> gnat
      case None                               => _dbGNATs -= key
    }
  }

  private[rspace] override def putDatum(txn: T, chs: Seq[C], datum: Datum[A]): Unit =
    withGNAT(hashChannels(chs)) { (key, gnatOpt) =>
      gnatOpt
        .map(gnat => gnat.copy(data = datum +: gnat.data))
        .orElse(GNAT(channels = chs, data = Seq(datum), wks = Seq.empty).some)
    }

  private[rspace] override def putWaitingContinuation(
      txn: T,
      chs: Seq[C],
      continuation: WaitingContinuation[P, K]): Unit =
    withGNAT(hashChannels(chs)) { (key, gnatOpt) =>
      gnatOpt
        .map(gnat => gnat.copy(wks = continuation +: gnat.wks))
        .orElse(GNAT(channels = chs, data = Seq.empty[Datum[A]], wks = Seq(continuation)).some)
    }

  private[rspace] override def addJoin(txn: T, c: C, cs: Seq[C]): Unit = {
    val existing: Seq[Seq[C]] = _dbJoins.remove(c).getOrElse(Seq.empty)
    if (!existing.exists(_.equals(cs)))
      _dbJoins.put(c, cs +: existing)
    else
      _dbJoins.put(c, existing)
  }

  private[rspace] override def removeDatum(txn: T, channels: Seq[C], index: Int): Unit = {
    val key = hashChannels(channels)
    withGNAT(key) { (_, gnatOpt) =>
      gnatOpt.map(gnat => gnat.copy(data = dropIndex(gnat.data, index)))
    }
  }

  private[rspace] override def removeWaitingContinuation(txn: T,
                                                         channels: Seq[C],
                                                         index: Int): Unit = {
    val key = hashChannels(channels)
    withGNAT(key) { (_, gnatOpt) =>
      gnatOpt.map(gnat => gnat.copy(wks = dropIndex(gnat.wks, index)))
    }
  }

  private[rspace] override def removeAll(txn: Unit, channels: Seq[C]): Unit = {
    withGNAT(hashChannels(channels)) { (key, gnatOpt) =>
      gnatOpt.map(_.copy(wks = Seq.empty, channels = Seq.empty))
    }
    for (c <- channels) removeJoin(txn, c, channels)
  }

  private[rspace] override def removeJoin(txn: T, c: C, cs: Seq[C]): Unit = {
    withGNAT(hashChannels(cs)) { (csKey, gnatOpt) =>
      if (gnatOpt.isEmpty || gnatOpt.get.wks.isEmpty) {
        val existing: Seq[Seq[C]] = _dbJoins.remove(c).getOrElse(Seq.empty)
        val filtered              = existing.filter(!_.equals(cs))
        if (filtered.nonEmpty)
          _dbJoins.put(c, filtered)
      }
      gnatOpt
    }
    collectGarbage(hashChannels(Seq(c)))
  }

  private[rspace] def removeAllJoins(txn: T, c: C): Unit = {
    _dbJoins.remove(c)
    collectGarbage(hashChannels(Seq(c)))
  }

  override def close(): Unit = ()

  override def clear(): Unit = {
    _dbGNATs.clear()
    _dbJoins.clear()
    eventsCounter.reset()
  }

  def getStoreCounters: StoreCounters = {
    val gnatsSize = _dbGNATs.foldLeft(0) {
      case (acc, (_, GNAT(chs, data, wks))) => acc + (chs.size + data.size + wks.size)
    }
    eventsCounter.createCounters(0, (gnatsSize + _dbJoins.size).toLong)
  }

  override def isEmpty: Boolean =
    _dbGNATs.isEmpty && _dbJoins.isEmpty

  override def toMap: Map[Seq[C], Row[P, A, K]] =
    _dbGNATs.map {
      case (_, GNAT(cs, data, wks)) =>
        (cs, Row(data, wks))
    }.toMap

  def isGNATOrphaned(gnat: GNAT[C, P, A, K]): Boolean =
    gnat.data.isEmpty && gnat.wks.isEmpty && !(gnat.channels.size == 1 && _dbJoins
      .contains(gnat.channels.head))

  private[this] def collectGarbage(key: H): Unit =
    withGNAT(key) { (_, gnatOpt) =>
      gnatOpt
    }
}

object InMemoryStore {

  def roundTrip[K: Serialize](k: K): K =
    Serialize[K].decode(Serialize[K].encode(k)) match {
      case Left(ex)     => throw ex
      case Right(value) => value
    }

  def hashBytes(bs: Array[Byte]): Array[Byte] =
    Blake2b256.hash(bs)

  def hashString(s: String): Array[Byte] =
    hashBytes(s.getBytes(StandardCharsets.UTF_8))

  def create[C, P, A, K <: Serializable](implicit sc: Serialize[C],
                                         sk: Serialize[K]): InMemoryStore[C, P, A, K] =
    new InMemoryStore[C, P, A, K](
      _dbGNATs = mutable.HashMap.empty[Blake2b256Hash, GNAT[C, P, A, K]],
      _dbJoins = mutable.HashMap.empty[C, Seq[Seq[C]]]
    )
}
