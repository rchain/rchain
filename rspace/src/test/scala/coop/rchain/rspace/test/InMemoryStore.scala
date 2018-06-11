package coop.rchain.rspace.test

import cats.implicits._
import coop.rchain.rspace._
import coop.rchain.rspace.history.ITrieStore
import coop.rchain.rspace.internal._
import coop.rchain.rspace.util.{dropIndex, removeFirst}
import coop.rchain.shared.AttemptOps._
import scodec.Codec
import scodec.bits.BitVector

import scala.collection.immutable.Seq
import scala.concurrent.SyncVar

trait Transaction[S] {
  def commit(): Unit
  def abort(): Unit
  def close(): Unit
  def readState[R](f: S => R): R
  def writeState[R](f: S => (S, R)): R

  def name: String
}

case class State[C, P, A, K](dbGNATs: Map[Blake2b256Hash, GNAT[C, P, A, K]],
                             dbJoins: Map[C, Seq[Seq[C]]]) {
  def isEmpty: Boolean =
    dbGNATs.isEmpty && dbJoins.isEmpty

  def size: Long =
    dbGNATs.foldLeft(0L) {
      case (acc, (_, GNAT(chs, data, wks))) => acc + (chs.size + data.size + wks.size)
    } + dbJoins.size
}

object State {
  def empty[C, P, A, K]: State[C, P, A, K] = State[C, P, A, K](Map.empty, Map.empty)
}

class InMemoryStore[C, P, A, K](
    val trieStore: ITrieStore[Transaction[State[C, P, A, K]], Blake2b256Hash, GNAT[C, P, A, K]]
)(implicit sc: Serialize[C], sk: Serialize[K])
    extends IStore[C, P, A, K] {
  val TRANSACTION_TIMEOUT = 100L

  private implicit val codecC: Codec[C] = sc.toCodec
  val eventsCounter: StoreEventsCounter = new StoreEventsCounter()

  private[this] val stateRef: SyncVar[State[C, P, A, K]] = {
    val sv = new SyncVar[StateType]
    sv.put(State.empty)
    sv
  }

  private[rspace] type T = Transaction[StateType]

  private[this] type StateType = State[C, P, A, K]

  private[rspace] def hashChannels(channels: Seq[C]): Blake2b256Hash =
    Codec[Seq[C]]
      .encode(channels)
      .map((bitVec: BitVector) => Blake2b256Hash.create(bitVec.toByteArray))
      .get

  private[rspace] def createTxnRead(): T = new Transaction[StateType] {

    val name: String = "read-" + Thread.currentThread().getId

    private[this] val state = stateRef.get(TRANSACTION_TIMEOUT).get

    override def commit(): Unit = {}
    override def abort(): Unit  = {}
    override def close(): Unit  = {}

    override def readState[R](f: StateType => R): R = f(state)

    override def writeState[R](f: StateType => (StateType, R)): R =
      throw new RuntimeException("read txn cannot write")
  }

  private[rspace] def createTxnWrite(): T =
    new Transaction[StateType] {
      val name: String = "write-" + Thread.currentThread().getId

      private[this] val initial = stateRef.take(TRANSACTION_TIMEOUT)
      private[this] var current = initial

      override def commit(): Unit =
        stateRef.put(current)

      override def abort(): Unit = stateRef.put(initial)

      override def close(): Unit = {}

      override def readState[R](f: StateType => R): R = f(current)

      override def writeState[R](f: StateType => (StateType, R)): R = {
        val (newState, result) = f(current)
        current = newState
        result
      }
    }

  private[rspace] def withTxn[R](txn: T)(f: T => R): R =
    try {
      val ret: R = f(txn)
      txn.commit()
      ret
    } catch {
      case ex: Throwable =>
        txn.abort()
        throw ex
    } finally {
      txn.close()
    }

  private[rspace] def getChannels(txn: T, key: Blake2b256Hash): Seq[C] =
    txn.readState(state => state.dbGNATs.get(key).map(_.channels).getOrElse(Seq.empty))

  private[rspace] def getData(txn: T, channels: Seq[C]): Seq[Datum[A]] =
    txn.readState(state =>
      state.dbGNATs.get(hashChannels(channels)).map(_.data).getOrElse(Seq.empty))

  private[this] def getMutableWaitingContinuation(
      txn: T,
      channels: Seq[C]): Seq[WaitingContinuation[P, K]] =
    txn.readState(
      _.dbGNATs
        .get(hashChannels(channels))
        .map(_.wks)
        .getOrElse(Seq.empty))

  private[rspace] def getWaitingContinuation(txn: T,
                                             channels: Seq[C]): Seq[WaitingContinuation[P, K]] =
    getMutableWaitingContinuation(txn, channels)
      .map { wk =>
        wk.copy(continuation = InMemoryStore.roundTrip(wk.continuation))
      }

  def getPatterns(txn: T, channels: Seq[C]): Seq[Seq[P]] =
    getMutableWaitingContinuation(txn, channels).map(_.patterns)

  private[rspace] def getJoin(txn: T, channel: C): Seq[Seq[C]] =
    txn.readState(_.dbJoins.getOrElse(channel, Seq.empty))

  private[this] def withGNAT(txn: T, key: Blake2b256Hash)(
      f: Option[GNAT[C, P, A, K]] => Option[GNAT[C, P, A, K]]): Unit =
    txn.writeState(state => {
      val gnatOpt   = state.dbGNATs.get(key)
      val resultOpt = f(gnatOpt)
      val ns        = handleGNATChange(state, key)(resultOpt)
      (ns, ())
    })

  private[this] def withJoin(txn: T, key: C)(f: Option[Seq[Seq[C]]] => Option[Seq[Seq[C]]]): Unit =
    txn.writeState(state => {
      val joinOpt   = state.dbJoins.get(key)
      val resultOpt = f(joinOpt)
      val ns        = handleJoinChange(state, key)(resultOpt)
      (ns, ())
    })

  private[this] def handleGNATChange(
      state: StateType,
      key: Blake2b256Hash): PartialFunction[Option[GNAT[C, P, A, K]], StateType] = {
    case Some(gnat) if !isOrphaned(gnat) => state.copy(dbGNATs = state.dbGNATs + (key -> gnat))
    case _                               => state.copy(dbGNATs = state.dbGNATs - key)
  }

  private[this] def handleJoinChange(state: StateType,
                                     key: C): PartialFunction[Option[Seq[Seq[C]]], StateType] = {
    case Some(join) => state.copy(dbJoins = state.dbJoins + (key -> join))
    case None       => state.copy(dbJoins = state.dbJoins - key)
  }

  private[rspace] def putDatum(txn: T, channels: Seq[C], datum: Datum[A]): Unit =
    withGNAT(txn, hashChannels(channels)) { gnatOpt =>
      gnatOpt
        .map(gnat => gnat.copy(data = datum +: gnat.data))
        .orElse(GNAT(channels = channels, data = Seq(datum), wks = Seq.empty).some)
    }

  private[rspace] def putWaitingContinuation(txn: T,
                                             channels: Seq[C],
                                             continuation: WaitingContinuation[P, K]): Unit =
    withGNAT(txn, hashChannels(channels)) { gnatOpt =>
      gnatOpt
        .map(gnat => gnat.copy(wks = continuation +: gnat.wks))
        .orElse(GNAT(channels = channels, data = Seq.empty, wks = Seq(continuation)).some)
    }

  private[rspace] def addJoin(txn: T, channel: C, channels: Seq[C]): Unit =
    withJoin(txn, channel) { joinOpt =>
      joinOpt
        .filter(!_.exists(_.equals(channels)))
        .map(channels +: _)
        .orElse(Seq(channels).some)
    }

  private[rspace] def removeDatum(txn: T, channels: Seq[C], index: Int): Unit =
    withGNAT(txn, hashChannels(channels)) { gnatOpt =>
      gnatOpt.map(gnat => gnat.copy(data = dropIndex(gnat.data, index)))
    }

  private[rspace] def removeWaitingContinuation(txn: T, channels: Seq[C], index: Int): Unit =
    withGNAT(txn, hashChannels(channels)) { gnatOpt =>
      gnatOpt.map(gnat => gnat.copy(wks = dropIndex(gnat.wks, index)))
    }

  private[rspace] def removeAll(txn: T, channels: Seq[C]): Unit = {
    withGNAT(txn, hashChannels(channels)) { gnatOpt =>
      gnatOpt.map(_.copy(wks = Seq.empty, channels = Seq.empty))
    }
    for (c <- channels) removeJoin(txn, c, channels)
  }

  private[rspace] def removeJoin(txn: T, channel: C, channels: Seq[C]): Unit =
    txn.readState { state =>
      val gnatOpt = state.dbGNATs.get(hashChannels(channels))
      if (gnatOpt.isEmpty || gnatOpt.get.wks.isEmpty) {
        withJoin(txn, channel) { joinOpt =>
          joinOpt
            .map(removeFirst(_)(_ == channels))
            .filter(_.nonEmpty)
        }
      }
    }

  private[rspace] override def removeAllJoins(txn: T, channel: C): Unit =
    withJoin(txn, channel)(_ => none)

  def close(): Unit = ()

  private[rspace] def clear(txn: T): Unit =
    txn.writeState(_ => {
      val ns: StateType = State.empty
      eventsCounter.reset()
      (ns, ())
    })

  def getStoreCounters: StoreCounters =
    withTxn(createTxnRead())(_.readState(state => eventsCounter.createCounters(0, state.size)))

  def isEmpty: Boolean = withTxn(createTxnRead())(_.readState(_.isEmpty))

  def toMap: Map[Seq[C], Row[P, A, K]] = withTxn(createTxnRead()) { txn =>
    txn.readState(_.dbGNATs.map {
      case (_, GNAT(cs, data, wks)) => (cs, Row(data, wks))
    })
  }

  private[this] def isOrphaned(gnat: GNAT[C, P, A, K]): Boolean =
    gnat.data.isEmpty && gnat.wks.isEmpty

  def getCheckpoint(): Blake2b256Hash = throw new Exception("unimplemented")

  private[rspace] def bulkInsert(txn: Transaction[StateType],
                                 gnats: Seq[(Blake2b256Hash, GNAT[C, P, A, K])]): Unit =
    ???
}

object InMemoryStore {

  def roundTrip[K: Serialize](k: K): K =
    Serialize[K].decode(Serialize[K].encode(k)) match {
      case Left(ex)     => throw ex
      case Right(value) => value
    }

  def create[C, P, A, K]()(implicit sc: Serialize[C], sk: Serialize[K]): InMemoryStore[C, P, A, K] =
    new InMemoryStore[C, P, A, K](
      trieStore =
        new DummyTrieStore[Transaction[State[C, P, A, K]], Blake2b256Hash, GNAT[C, P, A, K]])
}
