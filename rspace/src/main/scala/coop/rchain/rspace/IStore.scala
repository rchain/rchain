package coop.rchain.rspace

import java.util.concurrent.atomic.AtomicLong

import coop.rchain.rspace.history.{Branch, ITrieStore, TrieCache}
import coop.rchain.rspace.internal._
import coop.rchain.shared.SyncVarOps
import coop.rchain.shared.SyncVarOps._
import coop.rchain.rspace.util.canonicalize

import scala.Function.const
import scala.collection.immutable.Seq
import scala.concurrent.SyncVar

/** The interface for the underlying store
  *
  * @tparam C a type representing a channel
  * @tparam P a type representing a pattern
  * @tparam A a type representing an arbitrary piece of data
  * @tparam K a type representing a continuation
  */
trait IStore[C, P, A, K] {

  /**
    * The type of transactions
    */
  private[rspace] type Transaction

  private[rspace] type TrieTransaction

  private[rspace] type TrieStoreType =  ITrieStore[TrieTransaction, Blake2b256Hash, GNAT[C, P, A, K]]

  private[rspace] def createTxnRead(): Transaction

  private[rspace] def createTxnWrite(): Transaction

  private[rspace] def withTxn[R](txn: Transaction)(f: Transaction => R): R

  private[rspace] def hashChannels(channels: Seq[C]): Blake2b256Hash

  private[rspace] def getChannels(txn: Transaction, channelsHash: Blake2b256Hash): Seq[C]

  private[rspace] def putDatum(txn: Transaction, channels: Seq[C], datum: Datum[A]): Unit

  private[rspace] def getData(txn: Transaction, channels: Seq[C]): Seq[Datum[A]]

  private[rspace] def removeDatum(txn: Transaction, channel: Seq[C], index: Int): Unit

  private[rspace] def installWaitingContinuation(
      txn: Transaction,
      channels: Seq[C],
      continuation: WaitingContinuation[P, K]
  ): Unit

  private[rspace] def putWaitingContinuation(
      txn: Transaction,
      channels: Seq[C],
      continuation: WaitingContinuation[P, K]
  ): Unit

  private[rspace] def getWaitingContinuation(
      txn: Transaction,
      channels: Seq[C]
  ): Seq[WaitingContinuation[P, K]]

  private[rspace] def removeWaitingContinuation(
      txn: Transaction,
      channels: Seq[C],
      index: Int
  ): Unit

  private[rspace] def getPatterns(txn: Transaction, channels: Seq[C]): Seq[Seq[P]]

  private[rspace] def addJoin(txn: Transaction, channel: C, channels: Seq[C]): Unit

  private[rspace] def getJoin(txn: Transaction, channel: C): Seq[Seq[C]]

  private[rspace] def removeJoin(txn: Transaction, channel: C, channels: Seq[C]): Unit

  private[rspace] def joinMap: Map[Blake2b256Hash, Seq[Seq[C]]]

  def toMap: Map[Seq[C], Row[P, A, K]]

  private[rspace] def close(): Unit

  val trieStore: TrieStoreType

  val trieBranch: Branch

  def withTrieTxn[R](txn: Transaction)(f: TrieTransaction => R): R

  protected val _trieUpdates: SyncVar[Seq[TrieUpdate[C, P, A, K]]] =
    SyncVarOps.create(Seq.empty)

  def trieDelete(key: Blake2b256Hash, gnat: GNAT[C, P, A, K]) = {
    val count   = _trieUpdateCount.getAndIncrement()
    val currLog = _trieUpdates.take()
    _trieUpdates.put(currLog :+ TrieUpdate(count, Delete, key, gnat))
  }

  def trieInsert(key: Blake2b256Hash, gnat: GNAT[C, P, A, K]) = {
    val count   = _trieUpdateCount.getAndIncrement()
    val currLog = _trieUpdates.take()
    _trieUpdates.put(currLog :+ TrieUpdate(count, Insert, key, gnat))
  }

  private[rspace] def getTrieUpdates: Seq[TrieUpdate[C, P, A, K]] =
    _trieUpdates.get

  protected val _trieUpdateCount: AtomicLong = new AtomicLong(0L)

  private[rspace] def getTrieUpdateCount: Long =
    _trieUpdateCount.get()

  private[rspace] def clearTrieUpdates(): Unit = {
    _trieUpdates.update(const(Seq.empty))
    _trieUpdateCount.set(0L)
  }

  protected def processTrieUpdate(cacheStore: TrieStoreType, update: TrieUpdate[C, P, A, K]): Unit

  def createCheckpoint(): Blake2b256Hash = {
    val trieUpdates = _trieUpdates.take
    _trieUpdates.put(Seq.empty)
    _trieUpdateCount.set(0L)

    val timeStart = System.nanoTime()
    var timeInWriteTxn : Long = 0L

    val res = if(TrieCache.useCache) {
      val trieCache = new TrieCache(trieStore, trieBranch)
      collapse(trieUpdates).foreach(processTrieUpdate(trieCache, _))
      val rootHash = trieCache.withTxn(trieCache.createTxnRead()) { txn =>
        trieCache
          .persistAndGetRoot(txn, trieBranch)
          .getOrElse(throw new Exception("Could not get root hash"))
      }
      timeInWriteTxn = System.nanoTime()
      trieStore.withTxn(trieStore.createTxnWrite()) { txn =>
        trieStore.applyCache(txn, trieCache, rootHash)
      }
      rootHash
    } else {
      timeInWriteTxn = System.nanoTime()
      collapse(trieUpdates).foreach(processTrieUpdate(trieStore, _))
      trieStore.withTxn(trieStore.createTxnWrite()) { txn =>
        val rootHash = trieStore
          .persistAndGetRoot(txn, trieBranch)
          .getOrElse(throw new Exception("Could not get root hash"))
        rootHash
      }
    }
    val timeTotal = (System.nanoTime() - timeStart).asInstanceOf[Double] / 1000000000.0
    val timeInWriteTxnTotal = (System.nanoTime() - timeInWriteTxn).asInstanceOf[Double] / 1000000000.0
    println(s"createCheckpoint(), processed ${trieUpdates.length} trie updates, cache=${TrieCache.useCache}, time,sec=$timeTotal; writeTxnTime,sec=$timeInWriteTxnTotal")
    res
  }

  private[rspace] def collapse(in: Seq[TrieUpdate[C, P, A, K]]): Seq[TrieUpdate[C, P, A, K]] =
    in.groupBy(_.channelsHash)
      .flatMap {
        case (_, value) =>
          value
            .sorted(Ordering.by((tu: TrieUpdate[C, P, A, K]) => tu.count).reverse)
            .headOption match {
            case Some(TrieUpdate(_, Delete, _, _))          => List.empty
            case Some(insert @ TrieUpdate(_, Insert, _, _)) => List(insert)
            case _                                          => value
          }
      }
      .toList

  private[rspace] def bulkInsert(
      txn: Transaction,
      gnats: Seq[(Blake2b256Hash, GNAT[C, P, A, K])]
  ): Unit

  private[rspace] def clear(txn: Transaction): Unit

  def isEmpty: Boolean
}
