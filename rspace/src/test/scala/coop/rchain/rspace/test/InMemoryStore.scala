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

class InMemoryStore[C, P, A, K] private (
    dbGNATs: Map[Blake2b256Hash, GNAT[C, P, A, K]],
    dbJoins: Map[C, Seq[Seq[C]]]
)(implicit sc: Serialize[C], sk: Serialize[K])
    extends IStore[C, P, A, K]
    with ITestableStore[C, P] {

  private implicit val codecC: Codec[C] = sc.toCodec
  private implicit val codecK: Codec[K] = sk.toCodec

  val eventsCounter: StoreEventsCounter = new StoreEventsCounter()

  var _dbGNATs: Map[Blake2b256Hash, GNAT[C, P, A, K]] = dbGNATs
  var _dbJoins: Map[C, Seq[Seq[C]]]                   = dbJoins

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

  override def getPatterns(txn: T, channels: Seq[C]): Seq[Seq[P]] =
    getWK(txn, channels).map(_.patterns)

  private[rspace] override def getJoin(txn: T, c: C): Seq[Seq[C]] =
    _dbJoins.getOrElse(c, Seq.empty[Seq[C]])

  private[this] def withGNAT(txn: T, key: H)(
      f: (T, H, Option[GNAT[C, P, A, K]]) => Option[GNAT[C, P, A, K]]): Unit = {
    val gnatOpt = _dbGNATs.get(key)
    val rOpt    = f(txn, key, gnatOpt)
    handleChange(key)(rOpt)
  }

  private[this] def handleChange(key: H): PartialFunction[Option[GNAT[C, P, A, K]], Unit] = {
    case Some(gnat) if isOrphaned(gnat) => _dbGNATs -= key
    case Some(gnat)                     => _dbGNATs += key -> gnat
    case None                           => _dbGNATs -= key
  }

  private[rspace] override def putDatum(txn: T, chs: Seq[C], datum: Datum[A]): Unit =
    withGNAT(txn, hashChannels(chs)) { (_, key, gnatOpt) =>
      gnatOpt
        .map(gnat => gnat.copy(data = datum +: gnat.data))
        .orElse(GNAT(channels = chs, data = Seq(datum), wks = Seq.empty).some)
    }

  private[rspace] override def putWaitingContinuation(
      txn: T,
      chs: Seq[C],
      continuation: WaitingContinuation[P, K]): Unit =
    withGNAT(txn, hashChannels(chs)) { (_, key, gnatOpt) =>
      gnatOpt
        .map(gnat => gnat.copy(wks = continuation +: gnat.wks))
        .orElse(GNAT(channels = chs, data = Seq.empty[Datum[A]], wks = Seq(continuation)).some)
    }

  private[rspace] override def addJoin(txn: T, c: C, cs: Seq[C]): Unit = {
    val existing: Seq[Seq[C]] = _dbJoins.getOrElse(c, Seq.empty)
    if (!existing.exists(_.equals(cs)))
      _dbJoins += c -> (cs +: existing)
    else
      _dbJoins += c -> existing
  }

  private[rspace] override def removeDatum(txn: T, channels: Seq[C], index: Int): Unit =
    withGNAT(txn, hashChannels(channels)) { (_, _, gnatOpt) =>
      gnatOpt.map(gnat => gnat.copy(data = dropIndex(gnat.data, index)))
    }

  private[rspace] override def removeWaitingContinuation(txn: T,
                                                         channels: Seq[C],
                                                         index: Int): Unit = {
    val key = hashChannels(channels)
    withGNAT(txn, key) { (_, _, gnatOpt) =>
      gnatOpt.map(gnat => gnat.copy(wks = dropIndex(gnat.wks, index)))
    }
  }

  private[rspace] override def removeAll(txn: Unit, channels: Seq[C]): Unit = {
    withGNAT(txn, hashChannels(channels)) { (_, key, gnatOpt) =>
      gnatOpt.map(_.copy(wks = Seq.empty, channels = Seq.empty))
    }
    for (c <- channels) removeJoin(txn, c, channels)
  }

  private[rspace] override def removeJoin(txn: T, c: C, cs: Seq[C]): Unit = {
    withGNAT(txn, hashChannels(cs)) { (_, csKey, gnatOpt) =>
      if (gnatOpt.isEmpty || gnatOpt.get.wks.isEmpty) {
        val existing: Seq[Seq[C]] = _dbJoins.getOrElse(c, Seq.empty)
        val filtered              = existing.filter(!_.equals(cs))
        _dbJoins -= c
        if (filtered.nonEmpty) _dbJoins += c -> filtered

      }
      gnatOpt
    }
    collectGarbage(txn, hashChannels(Seq(c)))
  }

  private[rspace] override def removeAllJoins(txn: T, c: C): Unit = {
    _dbJoins -= c
    collectGarbage(txn, hashChannels(Seq(c)))
  }

  override def close(): Unit = ()

  override def clear(): Unit = withTxn(createTxnWrite()) { txn =>
    _dbGNATs = Map.empty
    _dbJoins = Map.empty
    eventsCounter.reset()
  }

  override def getStoreCounters: StoreCounters = withTxn(createTxnRead()) { txn =>
    val gnatsSize = _dbGNATs.foldLeft(0) {
      case (acc, (_, GNAT(chs, data, wks))) => acc + (chs.size + data.size + wks.size)
    }
    eventsCounter.createCounters(0, (gnatsSize + _dbJoins.size).toLong)
  }

  override def isEmpty: Boolean = withTxn(createTxnRead()) { txn =>
    _dbGNATs.isEmpty && _dbJoins.isEmpty
  }

  override def toMap: Map[Seq[C], Row[P, A, K]] = withTxn(createTxnRead()) { txn =>
    _dbGNATs.map {
      case (_, GNAT(cs, data, wks)) =>
        (cs, Row(data, wks))
    }.toMap
  }

  private[this] def isOrphaned(gnat: GNAT[C, P, A, K]): Boolean =
    gnat.data.isEmpty && gnat.wks.isEmpty && !(gnat.channels.size == 1 && _dbJoins
      .contains(gnat.channels.head))

  private[this] def collectGarbage(txn: T, key: H): Unit =
    withGNAT(txn, key) { (_, _, gnatOpt) =>
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
      dbGNATs = Map.empty[Blake2b256Hash, GNAT[C, P, A, K]],
      dbJoins = Map.empty[C, Seq[Seq[C]]]
    )
}
