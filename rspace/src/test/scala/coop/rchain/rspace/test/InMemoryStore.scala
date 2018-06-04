package coop.rchain.rspace.test

import cats.implicits._
import coop.rchain.rspace.history.{Blake2b256Hash, ITrieStore}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.util.{dropIndex, removeFirst}
import coop.rchain.rspace._
import coop.rchain.shared.AttemptOps._
import scodec.Codec
import scodec.bits.BitVector

import scala.collection.immutable.Seq
import scala.collection.mutable

class InMemoryStore[C, P, A, K] private (
    _dbGNATs: mutable.Map[Blake2b256Hash, GNAT[C, P, A, K]],
    _dbJoins: mutable.HashMap[C, Seq[Seq[C]]],
    val trieStore: ITrieStore[Unit, Blake2b256Hash, GNAT[C, P, A, K]]
)(implicit sc: Serialize[C], sk: Serialize[K])
    extends IStore[C, P, A, K] {

  private implicit val codecC: Codec[C] = sc.toCodec

  val eventsCounter: StoreEventsCounter = new StoreEventsCounter()

  private[rspace] type T = Unit

  private[rspace] def hashChannels(channels: Seq[C]): Blake2b256Hash =
    Codec[Seq[C]]
      .encode(channels)
      .map((bitVec: BitVector) => Blake2b256Hash.create(bitVec.toByteArray))
      .get

  private[rspace] def createTxnRead(): Unit = ()

  private[rspace] def createTxnWrite(): Unit = ()

  private[rspace] def withTxn[R](txn: T)(f: T => R): R =
    f(txn)

  private[rspace] def getChannels(txn: T, key: Blake2b256Hash) =
    _dbGNATs.get(key).map(_.channels).getOrElse(Seq.empty)

  private[rspace] def getData(txn: T, channels: Seq[C]): Seq[Datum[A]] =
    _dbGNATs.get(hashChannels(channels)).map(_.data).getOrElse(Seq.empty)

  private[this] def getMutableWaitingContinuation(
      txn: T,
      channels: Seq[C]): Seq[WaitingContinuation[P, K]] =
    _dbGNATs
      .get(hashChannels(channels))
      .map(_.wks)
      .getOrElse(Seq.empty)

  private[rspace] def getWaitingContinuation(txn: T,
                                             channels: Seq[C]): Seq[WaitingContinuation[P, K]] =
    getMutableWaitingContinuation(txn, channels)
      .map { wk =>
        wk.copy(continuation = InMemoryStore.roundTrip(wk.continuation))
      }

  def getPatterns(txn: T, channels: Seq[C]): Seq[Seq[P]] =
    getMutableWaitingContinuation(txn, channels).map(_.patterns)

  private[rspace] def getJoin(txn: T, channel: C): Seq[Seq[C]] =
    _dbJoins.getOrElse(channel, Seq.empty)

  private[this] def withGNAT(txn: T, key: Blake2b256Hash)(
      f: (Option[GNAT[C, P, A, K]]) => Option[GNAT[C, P, A, K]]): Unit = {
    val gnatOpt = _dbGNATs.get(key)
    val rOpt    = f(gnatOpt)
    handleChange(key)(rOpt)
  }

  private[this] def handleChange(
      key: Blake2b256Hash): PartialFunction[Option[GNAT[C, P, A, K]], Unit] = {
    case Some(gnat) if isOrphaned(gnat) => _dbGNATs -= key
    case Some(gnat)                     => _dbGNATs += key -> gnat
    case None                           => _dbGNATs -= key
  }

  private[rspace] def putDatum(txn: T, channels: Seq[C], datum: Datum[A]): Unit =
    withGNAT(txn, hashChannels(channels)) { (gnatOpt) =>
      gnatOpt
        .map(gnat => gnat.copy(data = datum +: gnat.data))
        .orElse(GNAT(channels = channels, data = Seq(datum), wks = Seq.empty).some)
    }

  private[rspace] def putWaitingContinuation(txn: T,
                                             channels: Seq[C],
                                             continuation: WaitingContinuation[P, K]): Unit =
    withGNAT(txn, hashChannels(channels)) { (gnatOpt) =>
      gnatOpt
        .map(gnat => gnat.copy(wks = continuation +: gnat.wks))
        .orElse(GNAT(channels = channels, data = Seq.empty, wks = Seq(continuation)).some)
    }

  private[rspace] def addJoin(txn: T, channel: C, channels: Seq[C]): Unit = {
    val existing: Seq[Seq[C]] = _dbJoins.remove(channel).getOrElse(Seq.empty)
    if (!existing.exists(_.equals(channels)))
      _dbJoins.put(channel, channels +: existing)
    else
      _dbJoins.put(channel, existing)
  }

  private[rspace] def removeDatum(txn: T, channels: Seq[C], index: Int): Unit =
    withGNAT(txn, hashChannels(channels)) { (gnatOpt) =>
      gnatOpt.map(gnat => gnat.copy(data = dropIndex(gnat.data, index)))
    }

  private[rspace] def removeWaitingContinuation(txn: T, channels: Seq[C], index: Int): Unit =
    withGNAT(txn, hashChannels(channels)) { (gnatOpt) =>
      gnatOpt.map(gnat => gnat.copy(wks = dropIndex(gnat.wks, index)))
    }

  private[rspace] def removeAll(txn: Unit, channels: Seq[C]): Unit = {
    withGNAT(txn, hashChannels(channels)) { (gnatOpt) =>
      gnatOpt.map(_.copy(wks = Seq.empty, channels = Seq.empty))
    }
    for (c <- channels) removeJoin(txn, c, channels)
  }

  private[rspace] def removeJoin(txn: T, channel: C, channels: Seq[C]): Unit = {
    withGNAT(txn, hashChannels(channels)) { (gnatOpt) =>
      if (gnatOpt.isEmpty || gnatOpt.get.wks.isEmpty) {
        val existing: Seq[Seq[C]] = _dbJoins.remove(channel).getOrElse(Seq.empty)
        val filtered              = removeFirst(existing)(_ == channels)
        if (filtered.nonEmpty) _dbJoins.put(channel, filtered)
      }
      gnatOpt
    }
    collectGarbage(txn, hashChannels(Seq(channel)))
  }

  private[rspace] def removeAllJoins(txn: T, channel: C): Unit = {
    _dbJoins.remove(channel)
    collectGarbage(txn, hashChannels(Seq(channel)))
  }

  def close(): Unit = ()

  def clear(t: T): Unit = {
    _dbGNATs.clear()
    _dbJoins.clear()
    eventsCounter.reset()
  }

  def getStoreCounters: StoreCounters = withTxn(createTxnRead()) { txn =>
    val gnatsSize = _dbGNATs.foldLeft(0) {
      case (acc, (_, GNAT(chs, data, wks))) => acc + (chs.size + data.size + wks.size)
    }
    eventsCounter.createCounters(0, (gnatsSize + _dbJoins.size).toLong)
  }

  def isEmpty: Boolean = withTxn(createTxnRead()) { txn =>
    _dbGNATs.isEmpty && _dbJoins.isEmpty
  }

  def toMap: Map[Seq[C], Row[P, A, K]] = withTxn(createTxnRead()) { txn =>
    _dbGNATs.map {
      case (_, GNAT(cs, data, wks)) =>
        (cs, Row(data, wks))
    }.toMap
  }

  private[this] def isOrphaned(gnat: GNAT[C, P, A, K]): Boolean =
    gnat.data.isEmpty && gnat.wks.isEmpty

  private[this] def collectGarbage(txn: T, key: Blake2b256Hash): Unit =
    withGNAT(txn, key) { (gnatOpt) =>
      gnatOpt
    }

  def getCheckpoint(): Blake2b256Hash = throw new Exception("unimplemented")

  private[rspace] def bulkInsert(txn: Unit, gnats: Seq[(Blake2b256Hash, GNAT[C, P, A, K])]): Unit =
    ???
}

object InMemoryStore {

  def roundTrip[K: Serialize](k: K): K =
    Serialize[K].decode(Serialize[K].encode(k)) match {
      case Left(ex)     => throw ex
      case Right(value) => value
    }

  def create[C, P, A, K <: Serializable](implicit sc: Serialize[C],
                                         sk: Serialize[K]): InMemoryStore[C, P, A, K] =
    new InMemoryStore[C, P, A, K](
      _dbGNATs = mutable.HashMap.empty[Blake2b256Hash, GNAT[C, P, A, K]],
      _dbJoins = mutable.HashMap.empty[C, Seq[Seq[C]]],
      trieStore = new DummyTrieStore[Unit, Blake2b256Hash, GNAT[C, P, A, K]]
    )
}
