package coop.rchain.rspace.test

import coop.rchain.rspace.internal._
import coop.rchain.rspace.test.ImmutableInMemStore.RichSyncVar
import coop.rchain.rspace.util.dropIndex
import coop.rchain.rspace.{IStore, ITestableStore, Serialize, StoreSize}
import javax.xml.bind.DatatypeConverter.printHexBinary

import scala.collection.immutable.Seq
import scala.concurrent.SyncVar
import scala.util.control.NonFatal

class ImmutableInMemStore[C, P, A, K <: Serializable] private (
    _keys: Map[String, Seq[C]],
    _waitingContinuations: Map[String, Seq[WaitingContinuation[P, K]]],
    _data: Map[String, Seq[Datum[A]]],
    _joinMap: Map[C, Set[String]],
)(implicit sc: Serialize[C], sk: Serialize[K])
    extends IStore[C, P, A, K]
    with ITestableStore[C, P] {

  private[this] val keysRef = new SyncVar[Map[String, Seq[C]]].init(_keys)
  private[this] val waitingContinuationsRef =
    new SyncVar[Map[String, Seq[WaitingContinuation[P, K]]]].init(_waitingContinuations)
  private[this] val dataRef = new SyncVar[Map[String, Seq[Datum[A]]]].init(_data)
  private[this] val joinRef = new SyncVar[Map[C, Set[String]]].init(_joinMap)

  private[rspace] type H = String

  private[rspace] type T = Unit

  private[rspace] def hashChannels(cs: Seq[C])(implicit sc: Serialize[C]): H =
    printHexBinary(InMemoryStore.hashBytes(cs.flatMap(sc.encode).toArray))

  private[this] def putCs(txn: T, channels: Seq[C]): Unit =
    keysRef.update(_ + (hashChannels(channels) -> channels))

  private[rspace] def getChannels(txn: T, s: H): Seq[C] =
    keysRef.get.getOrElse(s, Seq.empty[C])

  private[rspace] def createTxnRead(): Unit = ()

  private[rspace] def createTxnWrite(): Unit = ()

  private[rspace] def withTxn[R](txn: T)(f: T => R): R = f(txn)

  private[rspace] def collectGarbage(txn: T,
                                     channelsHash: H,
                                     dataCollected: Boolean = false,
                                     waitingContinuationsCollected: Boolean = false,
                                     joinsCollected: Boolean = false): Unit =
    collectGarbage(channelsHash)

  private[this] def collectGarbage(key: H): Unit =
    RichSyncVar.update3(waitingContinuationsRef, keysRef, dataRef) {
      (waitingContinuations, keys, data) =>
        val as    = data.get(key).exists(_.nonEmpty)
        val psks  = waitingContinuations.get(key).exists(_.nonEmpty)
        val cs    = keys.getOrElse(key, Seq.empty[C])
        val joins = cs.size == 1 && joinRef.get.contains(cs.head)
        (
          Some(waitingContinuations)
            .filter(_ => psks)
            .getOrElse(waitingContinuations - key),
          Some(keys)
            .filter(_ => as || psks || joins)
            .getOrElse(keys - key),
          Some(data)
            .filter(_ => as)
            .getOrElse(data - key)
        )
    }

  private[rspace] def putDatum(txn: T, channels: Seq[C], datum: Datum[A]): Unit =
    dataRef.update { data =>
      val key = hashChannels(channels)
      putCs(txn, channels)
      val datums = data.getOrElse(key, Seq.empty[Datum[A]])
      data + (key -> (datum +: datums))
    }

  private[rspace] def putWaitingContinuation(txn: T,
                                             channels: Seq[C],
                                             continuation: WaitingContinuation[P, K]): Unit =
    waitingContinuationsRef.update { waitingContinuations =>
      val key = hashChannels(channels)
      putCs(txn, channels)
      val forKey: Seq[WaitingContinuation[P, K]] =
        waitingContinuations.getOrElse(key, Seq.empty[WaitingContinuation[P, K]])
      waitingContinuations + (key -> (continuation +: forKey))
    }

  private[rspace] def getData(txn: T, channels: Seq[C]): Seq[Datum[A]] =
    dataRef.get.getOrElse(hashChannels(channels), Seq.empty[Datum[A]])

  private[rspace] def getWaitingContinuation(txn: T, curr: Seq[C]): Seq[WaitingContinuation[P, K]] =
    waitingContinuationsRef.get
      .getOrElse(hashChannels(curr), Seq.empty[WaitingContinuation[P, K]])
      .map { (wk: WaitingContinuation[P, K]) =>
        wk.copy(continuation = ImmutableInMemStore.roundTrip(wk.continuation))
      }

  private[rspace] def removeDatum(txn: T, channel: C, index: Int): Unit =
    removeDatum(txn, Seq(channel), index)

  private[rspace] def removeDatum(txn: T, channels: Seq[C], index: Int): Unit = {
    val key = hashChannels(channels)
    dataRef.update(data =>
      data.get(key).map(as => data + (key -> dropIndex(as, index))).getOrElse(data))
    collectGarbage(key)
  }

  private[rspace] def removeWaitingContinuation(txn: T, channels: Seq[C], index: Int): Unit = {
    val key = hashChannels(channels)
    waitingContinuationsRef.update(wc =>
      wc.get(key).map(x => wc + (key -> dropIndex(x, index))).getOrElse(wc))
    collectGarbage(key)
  }

  //how volatile is this?
  private[rspace] def removeAll(txn: Unit, channels: Seq[C]): Unit = {
    val key = hashChannels(channels)
    RichSyncVar.update2(waitingContinuationsRef, dataRef) { (waitingContinuations, data) =>
      (waitingContinuations + (key -> Seq.empty), data + (key -> Seq.empty))
    }
    for (c <- channels) removeJoin(txn, c, channels)
  }

  private[rspace] def addJoin(txn: T, c: C, cs: Seq[C]): Unit =
    joinRef.update(joins => joins + (c -> (joins.getOrElse(c, Set.empty) + hashChannels(cs))))

  private[rspace] def getJoin(txn: T, c: C): Seq[Seq[C]] =
    joinRef.get.getOrElse(c, Set.empty[String]).toList.map(getChannels(txn, _))

  private[rspace] def removeJoin(txn: T, c: C, cs: Seq[C]): Unit = {
    val joinKey = hashChannels(Seq(c))
    val csKey   = hashChannels(cs)
    RichSyncVar.update2(waitingContinuationsRef, joinRef) { (waitingContinuations, joins) =>
      val hasContinuationValues = waitingContinuations.get(csKey).forall(_.isEmpty)
      def dropKey =
        (value: Set[String]) =>
          value - csKey match {
            case r if r.isEmpty => joins - c
            case removed        => joins + (c -> removed)
        }
      val result =
        Option(joins).filter(_ => !hasContinuationValues).getOrElse {
          joins
            .get(c)
            .map(dropKey)
            .getOrElse(joins)
        }
      (waitingContinuations, result)
    }
    collectGarbage(joinKey)
  }

  private[rspace] def removeAllJoins(txn: T, c: C): Unit = {
    joinRef.update(_ - c)
    collectGarbage(hashChannels(Seq(c)))
  }

  def close(): Unit = ()

  def getPatterns(txn: T, channels: Seq[C]): Seq[Seq[P]] =
    waitingContinuationsRef.get.getOrElse(hashChannels(channels), Nil).map(_.patterns)

  def clear(): Unit = {
    keysRef.replace(Map.empty[String, Seq[C]])
    waitingContinuationsRef.replace(Map.empty[String, Seq[WaitingContinuation[P, K]]])
    dataRef.replace(Map.empty[String, Seq[Datum[A]]])
    joinRef.replace(Map.empty[C, Set[String]])
  }

  def getStoreSize: StoreSize =
    StoreSize(0,
              (keysRef.get.size +
                waitingContinuationsRef.get.size +
                dataRef.get.size +
                joinRef.get.size).toLong)

  def isEmpty: Boolean =
    waitingContinuationsRef.get.isEmpty && dataRef.get.isEmpty && keysRef.get.isEmpty && joinRef.get.isEmpty

  def toMap: Map[Seq[C], Row[P, A, K]] =
    keysRef.get
      .map {
        case (hash, cs) =>
          val data = dataRef.get.getOrElse(hash, Seq.empty[Datum[A]])
          val wks =
            waitingContinuationsRef.get.getOrElse(hash, Seq.empty[WaitingContinuation[P, K]])
          (cs, Row(data, wks))
      }
}

object ImmutableInMemStore {
  val DEFAULT_TIMEOUT_MS: Long = 100

  def roundTrip[K: Serialize](k: K): K =
    Serialize[K].decode(Serialize[K].encode(k)) match {
      case Left(ex)     => throw ex
      case Right(value) => value
    }

  def create[C, P, A, K <: Serializable](implicit sc: Serialize[C],
                                         sk: Serialize[K]): ImmutableInMemStore[C, P, A, K] =
    new ImmutableInMemStore[C, P, A, K](
      _keys = Map.empty[String, Seq[C]],
      _waitingContinuations = Map.empty[String, Seq[WaitingContinuation[P, K]]],
      _data = Map.empty[String, Seq[Datum[A]]],
      _joinMap = Map.empty[C, Set[String]]
    )

  object RichSyncVar {
    def update3[R1, R2, R3](ref1: SyncVar[R1], ref2: SyncVar[R2], ref3: SyncVar[R3])(
        f: (R1, R2, R3) => (R1, R2, R3)): Unit = {
      val prev1 = ref1.take(DEFAULT_TIMEOUT_MS)
      try {
        val prev2 = ref2.take(DEFAULT_TIMEOUT_MS)
        try {
          val prev3 = ref3.take(DEFAULT_TIMEOUT_MS)
          try {
            val (next1, next2, next3) = f(prev1, prev2, prev3)
            ref1.put(next1)
            ref2.put(next2)
            ref3.put(next3)
          } catch {
            case ex: Throwable =>
              ref3.put(prev3)
              throw ex
          }
        } catch {
          case ex: Throwable =>
            ref2.put(prev2)
            throw ex
        }
      } catch {
        case ex: Throwable =>
          ref1.put(prev1)
          throw ex
      }
    }

    def update2[R1, R2](ref1: SyncVar[R1], ref2: SyncVar[R2])(f: (R1, R2) => (R1, R2)): Unit = {
      val prev1 = ref1.take(DEFAULT_TIMEOUT_MS)
      try {
        val prev2 = ref2.take(DEFAULT_TIMEOUT_MS)
        try {
          val (next1, next2) = f(prev1, prev2)
          ref1.put(next1)
          ref2.put(next2)
        } catch {
          case ex: Throwable =>
            ref2.put(prev2)
            throw ex
        }
      } catch {
        case ex: Throwable =>
          ref1.put(prev1)
          throw ex
      }
    }
  }

  implicit class RichSyncVar[R](ref: SyncVar[R]) {

    def update(f: R => R): SyncVar[R] = {
      val prev = ref.take(DEFAULT_TIMEOUT_MS)
      try {
        val next = f(prev)
        ref.put(next)
      } catch {
        // simulate rollback
        case ex: Throwable =>
          ref.put(prev)
          throw ex
      }
      ref
    }

    def init(r: R): SyncVar[R] = {
      ref.put(r)
      ref
    }

    def replace(r: R): SyncVar[R] =
      update(_ => r)
  }

}
