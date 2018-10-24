package coop.rchain.rspace

import coop.rchain.rspace.history.{Branch, ITrieStore}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.util.canonicalize
import coop.rchain.shared.SeqOps.{dropIndex, removeFirst}
import kamon._
import scodec.Codec

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.Seq

/**
  * This implementation of Transaction exists only to satisfy the requirements of IStore.
  * Ideally this can be dropped after InMemoryStore is removed.
  */
class NoopTxn[S] extends InMemTransaction[S] {
  override def commit(): Unit                   = ()
  override def abort(): Unit                    = ()
  override def close(): Unit                    = ()
  override def readState[R](f: S => R): R       = ???
  override def writeState[R](f: S => (S, R)): R = ???
  override def name: String                     = "noop"
}

/**
  * This store is an optimized version of IStore.
  * It does not handle high level locking.
  *
  * It should be used with RSpace that solves high level locking (e.g. FineGrainedRSpace).
  */
class LockFreeInMemoryStore[T, C, P, A, K](
    val trieStore: ITrieStore[T, Blake2b256Hash, GNAT[C, P, A, K]],
    val trieBranch: Branch
)(implicit sc: Serialize[C], sp: Serialize[P], sa: Serialize[A], sk: Serialize[K])
    extends IStore[C, P, A, K]
    with CloseOps {

  protected[rspace] type Transaction = InMemTransaction[State[C, P, A, K]]

  override private[rspace] def createTxnRead(): Transaction = {
    failIfClosed()
    new NoopTxn[State[C, P, A, K]]
  }
  override private[rspace] def createTxnWrite(): Transaction = {
    failIfClosed()
    new NoopTxn[State[C, P, A, K]]
  }
  override private[rspace] def withTxn[R](txn: Transaction)(f: Transaction => R) = f(txn)
  override def close(): Unit                                                     = super.close()

  type TrieTransaction = T

  private implicit val codecC: Codec[C] = sc.toCodec
  private implicit val codecP: Codec[P] = sp.toCodec
  private implicit val codecA: Codec[A] = sa.toCodec
  private implicit val codecK: Codec[K] = sk.toCodec

  private val stateGNAT: TrieMap[Blake2b256Hash, GNAT[C, P, A, K]] =
    TrieMap[Blake2b256Hash, GNAT[C, P, A, K]]()
  private val stateJoin: TrieMap[C, Seq[Seq[C]]] = TrieMap[C, Seq[Seq[C]]]()

  private[this] val refine       = Map("path" -> "inmem")
  private[this] val entriesGauge = Kamon.gauge("entries").refine(refine)

  private[rspace] def updateGauges() =
    entriesGauge.set(stateGNAT.readOnlySnapshot.size.toLong)

  private[rspace] def hashChannels(channels: Seq[C]): Blake2b256Hash =
    StableHashProvider.hash(channels)

  override def withTrieTxn[R](txn: Transaction)(f: TrieTransaction => R): R =
    trieStore.withTxn(trieStore.createTxnWrite()) { ttxn =>
      f(ttxn)
    }

  private[rspace] def getChannels(txn: Transaction, key: Blake2b256Hash): Seq[C] =
    stateGNAT.get(key).map(_.channels).getOrElse(Seq.empty)

  private[rspace] def getData(txn: Transaction, channels: Seq[C]): Seq[Datum[A]] =
    stateGNAT.get(hashChannels(channels)).map(_.data).getOrElse(Seq.empty)

  private[this] def getMutableWaitingContinuation(
      txn: Transaction,
      channels: Seq[C]
  ): Seq[WaitingContinuation[P, K]] =
    stateGNAT
      .get(hashChannels(channels))
      .map(_.wks)
      .getOrElse(Seq.empty)

  private[rspace] def getWaitingContinuation(
      txn: Transaction,
      channels: Seq[C]
  ): Seq[WaitingContinuation[P, K]] =
    getMutableWaitingContinuation(txn, channels)
      .map { wk =>
        wk.copy(continuation = InMemoryStore.roundTrip(wk.continuation))
      }

  def getPatterns(txn: Transaction, channels: Seq[C]): Seq[Seq[P]] =
    getMutableWaitingContinuation(txn, channels).map(_.patterns)

  private[rspace] def getJoin(txn: Transaction, channel: C): Seq[Seq[C]] =
    stateJoin.getOrElse(channel, Seq.empty)

  private[rspace] def joinMap: Map[Blake2b256Hash, Seq[Seq[C]]] =
    stateJoin.map {
      case (k, v) => (Blake2b256Hash.create(Codec[C].encode(k).map(_.toByteArray).get), v)
    }.toMap

  private[rspace] def putDatum(txn: Transaction, channels: Seq[C], datum: Datum[A]): Unit = {
    val hash = hashChannels(channels)
    val v = stateGNAT
      .get(hash)
      .map { gnat =>
        gnat.copy(data = datum +: gnat.data)
      }
      .getOrElse(GNAT(channels = channels, data = Seq(datum), wks = Seq.empty))
    stateGNAT.put(hash, v)
    trieInsert(hash, v)
  }

  private[rspace] def putWaitingContinuation(
      txn: Transaction,
      channels: Seq[C],
      continuation: WaitingContinuation[P, K]
  ): Unit = {
    val hash = hashChannels(channels)
    val v = stateGNAT
      .get(hash)
      .map { gnat =>
        gnat.copy(wks = continuation +: gnat.wks)
      }
      .getOrElse(GNAT(channels = channels, data = Seq.empty, wks = Seq(continuation)))
    stateGNAT.put(hash, v)
    trieInsert(hash, v)
  }

  private[rspace] def addJoin(txn: Transaction, channel: C, channels: Seq[C]): Unit =
    stateJoin
      .get(channel) match {
      case Some(joins) if !joins.contains(channels) => stateJoin.put(channel, channels +: joins)
      case None                                     => stateJoin.put(channel, Seq(channels))
      case _                                        => ()
    }

  private[rspace] def removeDatum(txn: Transaction, channels: Seq[C], index: Int): Unit = {
    val hash = hashChannels(channels)
    stateGNAT
      .get(hash)
      .map { gnat =>
        gnat.copy(data = dropIndex(gnat.data, index))
      }
      .foreach { v =>
        if (!isOrphaned(v)) {
          stateGNAT.put(hash, v)
          trieInsert(hash, v)
        } else {
          stateGNAT.remove(hash)
          trieDelete(hash, v)
        }
      }
  }

  private[rspace] def removeWaitingContinuation(
      txn: Transaction,
      channels: Seq[C],
      index: Int
  ): Unit = {
    val hash = hashChannels(channels)
    stateGNAT
      .get(hash)
      .map { gnat =>
        gnat.copy(wks = dropIndex(gnat.wks, index))
      }
      .foreach { v =>
        if (!isOrphaned(v)) {
          stateGNAT.put(hash, v)
          trieInsert(hash, v)
        } else {
          stateGNAT.remove(hash)
          trieDelete(hash, v)
        }
      }
  }

  private[rspace] def removeJoin(txn: Transaction, channel: C, channels: Seq[C]): Unit = {
    val gnatOpt = stateGNAT.get(hashChannels(channels))
    if (gnatOpt.isEmpty || gnatOpt.get.wks.isEmpty) {
      stateJoin
        .get(channel)
        .map(removeFirst(_)(_ == channels))
        .filter(_.nonEmpty) match {
        case Some(value) => stateJoin.put(channel, value)
        case None        => stateJoin.remove(channel)
      }

    }
  }

  private[rspace] def clear(txn: Transaction): Unit = {
    stateJoin.clear()
    stateGNAT.clear()
  }

  def isEmpty: Boolean = stateGNAT.isEmpty && stateJoin.isEmpty

  def toMap: Map[Seq[C], Row[P, A, K]] =
    stateGNAT.readOnlySnapshot.map {
      case (_, GNAT(cs, data, wks)) => (cs, Row(data, wks))
    }.toMap

  private[this] def isOrphaned(gnat: GNAT[C, P, A, K]): Boolean =
    gnat.data.isEmpty && gnat.wks.isEmpty

  protected def processTrieUpdate(update: TrieUpdate[C, P, A, K]): Unit =
    update match {
      case TrieUpdate(_, Insert, channelsHash, gnat) =>
        history.insert(trieStore, trieBranch, channelsHash, canonicalize(gnat))
      case TrieUpdate(_, Delete, channelsHash, gnat) =>
        history.delete(trieStore, trieBranch, channelsHash, canonicalize(gnat))
    }

  private[rspace] def bulkInsert(
      txn: Transaction,
      gnats: Seq[(Blake2b256Hash, GNAT[C, P, A, K])]
  ): Unit =
    gnats.foreach {
      case (hash, gnat @ GNAT(channels, _, wks)) =>
        stateGNAT.put(hash, gnat)
        for {
          wk      <- wks
          channel <- channels
        } {
          addJoin(txn, channel, channels)
        }
    }

  private[rspace] def installWaitingContinuation(
      txn: Transaction,
      channels: Seq[C],
      continuation: WaitingContinuation[P, K]
  ): Unit = {
    val key  = hashChannels(channels)
    val gnat = GNAT[C, P, A, K](channels, Seq.empty, Seq(continuation))
    stateGNAT.put(key, gnat)
  }
}

object LockFreeInMemoryStore {

  def create[T, C, P, A, K](
      trieStore: ITrieStore[T, Blake2b256Hash, GNAT[C, P, A, K]],
      branch: Branch
  )(
      implicit sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K]
  ): LockFreeInMemoryStore[T, C, P, A, K] =
    new LockFreeInMemoryStore[T, C, P, A, K](trieStore, branch)(sc, sp, sa, sk)
}
