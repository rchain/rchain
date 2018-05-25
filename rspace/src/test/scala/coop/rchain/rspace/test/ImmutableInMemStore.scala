package coop.rchain.rspace.test

import java.util.concurrent.atomic.AtomicReference

import coop.rchain.rspace.internal._
import coop.rchain.rspace.util.dropIndex
import coop.rchain.rspace.{IStore, ITestableStore, Serialize}
import javax.xml.bind.DatatypeConverter.printHexBinary

import scala.collection.immutable.Seq

class ImmutableInMemStore[C, P, A, K <: Serializable] private (
    _keys: Map[String, Seq[C]],
    _waitingContinuations: Map[String, Seq[WaitingContinuation[P, K]]],
    _data: Map[String, Seq[Datum[A]]],
    _joinMap: Map[C, Set[String]],
)(implicit sc: Serialize[C])
    extends IStore[C, P, A, K]
    with ITestableStore[C, P] {

  val keysRef = new AtomicReference[Map[String, Seq[C]]](_keys)
  val waitingContinuationsRef =
    new AtomicReference[Map[String, Seq[WaitingContinuation[P, K]]]](_waitingContinuations)
  val dataRef = new AtomicReference[Map[String, Seq[Datum[A]]]](_data)
  val joinRef = new AtomicReference[Map[C, Set[String]]](_joinMap)

  private[rspace] type H = String

  private[rspace] type T = Unit

  private[rspace] def hashChannels(cs: Seq[C])(implicit sc: Serialize[C]): H =
    printHexBinary(InMemoryStore.hashBytes(cs.flatMap(sc.encode).toArray))

  private[this] def putCs(txn: T, channels: Seq[C]): Unit =
    keysRef.updateAndGet(_ + (hashChannels(channels) -> channels))

  private[rspace] def getChannels(txn: T, s: H) =
    keysRef.get().getOrElse(s, Seq.empty[C])

  private[rspace] def createTxnRead(): Unit = ()

  private[rspace] def createTxnWrite(): Unit = ()

  private[rspace] def withTxn[R](txn: T)(f: T => R): R =
    f(txn)

  def collectGarbage(key: H): Unit = {
    val as = dataRef.get().get(key).exists(_.nonEmpty)
    if (!as) {
      //we still may have empty list, remove it as well
      dataRef.updateAndGet(_ - key)
    }

    val psks = waitingContinuationsRef.get().get(key).exists(_.nonEmpty)
    if (!psks) {
      //we still may have empty list, remove it as well
      waitingContinuationsRef.updateAndGet(_ - key)
    }

    val cs    = keysRef.get().getOrElse(key, Seq.empty[C])
    val joins = cs.size == 1 && joinRef.get().contains(cs.head)

    if (!as && !psks && !joins) {
      keysRef.updateAndGet(_ - key)
    }
  }

  private[rspace] def putDatum(txn: T, channels: Seq[C], datum: Datum[A]): Unit = {
    val key = hashChannels(channels)
    putCs(txn, channels)

    dataRef.updateAndGet(data => {
      val datums = data.getOrElse(key, Seq.empty[Datum[A]])
      data + (key -> (datum +: datums))
    })
  }

  private[rspace] def putWaitingContinuation(txn: T,
                                             channels: Seq[C],
                                             continuation: WaitingContinuation[P, K]): Unit = {
    val key = hashChannels(channels)
    putCs(txn, channels)
    waitingContinuationsRef.updateAndGet(wc => {
      wc.get(key) match {
        case Some(list) => wc + (key -> (list :+ continuation))
        case None       => wc + (key -> (Seq.empty :+ continuation))
      }
    })
  }

  private[rspace] def getData(txn: T, channels: Seq[C]): Seq[Datum[A]] =
    dataRef.get().getOrElse(hashChannels(channels), Seq.empty[Datum[A]])

  private[rspace] def getWaitingContinuation(txn: T, curr: Seq[C]): Seq[WaitingContinuation[P, K]] =
    waitingContinuationsRef
      .get()
      .getOrElse(hashChannels(curr), Seq.empty[WaitingContinuation[P, K]])
      .map { (wk: WaitingContinuation[P, K]) =>
        wk.copy(continuation = InMemoryStore.roundTrip(wk.continuation))
      }

  private[rspace] def removeDatum(txn: T, channel: C, index: Int): Unit =
    removeDatum(txn, Seq(channel), index)

  private[rspace] def removeDatum(txn: T, channels: Seq[C], index: Int): Unit = {
    val key = hashChannels(channels)
    dataRef.updateAndGet(data => {
      data.get(key).map(as => data + (key -> dropIndex(as, index))).getOrElse(data)
    })
    collectGarbage(key)
  }

  private[rspace] def removeWaitingContinuation(txn: T, channels: Seq[C], index: Int): Unit = {
    val key = hashChannels(channels)
    waitingContinuationsRef.updateAndGet(wc => {
      wc.get(key).map(x => wc + (key -> dropIndex(x, index))).getOrElse(wc)
    })
    collectGarbage(key)
  }

  private[rspace] def removeAll(txn: Unit, channels: Seq[C]): Unit = {
    val key = hashChannels(channels)
    dataRef.updateAndGet(_ + (key -> Seq.empty))
    waitingContinuationsRef.updateAndGet(wc => wc + (key -> Seq.empty))
    for (c <- channels) removeJoin(txn, c, channels)
  }

  private[rspace] def addJoin(txn: T, c: C, cs: Seq[C]): Unit =
    joinRef.updateAndGet(joins => {
      joins.get(c) match {
        case Some(value) => joins + (c -> (value + hashChannels(cs)))
        case None        => joins + (c -> (Set.empty + hashChannels(cs)))
      }
    })

  private[rspace] def getJoin(txn: T, c: C): Seq[Seq[C]] =
    joinRef.get().getOrElse(c, Set.empty[String]).toList.map(getChannels(txn, _))

  private[rspace] def removeJoin(txn: T, c: C, cs: Seq[C]): Unit = {
    val joinKey = hashChannels(Seq(c))
    val csKey   = hashChannels(cs)
    if (waitingContinuationsRef.get().get(csKey).forall(_.isEmpty)) {
      joinRef.updateAndGet(joins => {
        joins.get(c) match {
          case Some(value) => {
            value - csKey match {
              case r if r.isEmpty => joins - c
              case removed        => joins + (c -> removed)
            }
          }
          case None => joins
        }
      })
    }
    collectGarbage(joinKey)
  }

  private[rspace] def removeAllJoins(txn: T, c: C): Unit = {
    joinRef.updateAndGet(_ - c)
    collectGarbage(hashChannels(Seq(c)))
  }

  def close(): Unit = ()

  def getPatterns(txn: T, channels: Seq[C]): Seq[Seq[P]] =
    waitingContinuationsRef.get().getOrElse(hashChannels(channels), Nil).map(_.patterns)

  def clear(): Unit = {
    keysRef.set(Map.empty[String, Seq[C]])
    waitingContinuationsRef.set(Map.empty[String, Seq[WaitingContinuation[P, K]]])
    dataRef.set(Map.empty[String, Seq[Datum[A]]])
    joinRef.set(Map.empty[C, Set[String]])
  }

  def isEmpty: Boolean =
    waitingContinuationsRef.get().isEmpty && dataRef.get().isEmpty && keysRef
      .get()
      .isEmpty && joinRef.get().isEmpty

  def toMap: Map[Seq[C], Row[P, A, K]] =
    keysRef
      .get()
      .map {
        case (hash, cs) =>
          val data = dataRef.get().getOrElse(hash, Seq.empty[Datum[A]])
          val wks =
            waitingContinuationsRef.get().getOrElse(hash, Seq.empty[WaitingContinuation[P, K]])
          (cs, Row(data, wks))
      }
}

object ImmutableInMemStore {
  def create[C, P, A, K <: Serializable](
      implicit sc: Serialize[C]): ImmutableInMemStore[C, P, A, K] =
    new ImmutableInMemStore[C, P, A, K](
      _keys = Map.empty[String, Seq[C]],
      _waitingContinuations = Map.empty[String, Seq[WaitingContinuation[P, K]]],
      _data = Map.empty[String, Seq[Datum[A]]],
      _joinMap = Map.empty[C, Set[String]]
    )
}
