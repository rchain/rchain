package coop.rchain.rspace

import coop.rchain.rspace.history.{Branch, ITrieStore}
import coop.rchain.rspace.internal._
import monix.execution.atomic.AtomicAny

import scala.Function.const
import scala.collection.concurrent.TrieMap
import scala.collection.immutable.Seq

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

  val trieStore: ITrieStore[TrieTransaction, Blake2b256Hash, GNAT[C, P, A, K]]

  val trieBranch: Branch

  def withTrieTxn[R](txn: Transaction)(f: TrieTransaction => R): R

  /**
    * high level locking is solved by RSpace
    */
  private val _trieUpdates: TrieMap[Blake2b256Hash, List[TrieUpdate[C, P, A, K]]] = TrieMap.empty

  def trieDelete(key: Blake2b256Hash, gnat: GNAT[C, P, A, K]): Unit = {
    val value = TrieUpdate(Delete, key, gnat)
    _trieUpdates.putIfAbsent(key, value :: Nil) match {
      case Some(x) => _trieUpdates.replace(key, x, value :: x)
      case None    => ()
    }
  }

  def trieInsert(key: Blake2b256Hash, gnat: GNAT[C, P, A, K]): Unit = {
    val value = TrieUpdate(Insert, key, gnat)
    _trieUpdates.putIfAbsent(key, value :: Nil) match {
      case Some(x) => _trieUpdates.replace(key, x, value :: x)
      case None    => ()
    }
  }

  private[rspace] def getTrieUpdates: Map[Blake2b256Hash, List[TrieUpdate[C, P, A, K]]] =
    _trieUpdates.toMap

  protected def processTrieUpdate(update: TrieUpdate[C, P, A, K]): Unit

  private[rspace] def getAndClearTrieUpdates()
    : Map[Blake2b256Hash, List[TrieUpdate[C, P, A, K]]] = {
    val r = getTrieUpdates
    _trieUpdates.clear()
    r
  }

  def createCheckpoint(): Blake2b256Hash = {
    val trieUpdates = getAndClearTrieUpdates()
    IStore.collapse(trieUpdates).foreach(processTrieUpdate)
    trieStore.withTxn(trieStore.createTxnWrite()) { txn =>
      trieStore
        .persistAndGetRoot(txn, trieBranch)
        .getOrElse(throw new Exception("Could not get root hash"))
    }
  }

  private[rspace] def bulkInsert(
      txn: Transaction,
      gnats: Seq[(Blake2b256Hash, GNAT[C, P, A, K])]
  ): Unit

  private[rspace] def clear(txn: Transaction): Unit

  def isEmpty: Boolean
}

object IStore {
  private[rspace] def collapse[C, P, A, K](
      in: Map[Blake2b256Hash, List[TrieUpdate[C, P, A, K]]]
  ): Seq[TrieUpdate[C, P, A, K]] =
    in.flatMap {
      case (_, value) =>
        value.headOption match {
          case Some(TrieUpdate(Delete, _, _))          => List.empty
          case Some(insert @ TrieUpdate(Insert, _, _)) => List(insert)
          case _                                       => value
        }
    }.toList
}
