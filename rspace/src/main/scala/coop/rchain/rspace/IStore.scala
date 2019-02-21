package coop.rchain.rspace

import cats.Traverse
import cats.effect.Sync
import cats.implicits._
import coop.rchain.catscontrib.ski._
import coop.rchain.catscontrib.traverseSeq
import coop.rchain.rspace.history.{Branch, ITrieStore}
import coop.rchain.rspace.internal._
import monix.execution.atomic.AtomicAny

import scala.collection.immutable.Seq

/** The interface for the underlying store
  *
  * @tparam C a type representing a channel
  * @tparam P a type representing a pattern
  * @tparam A a type representing an arbitrary piece of data
  * @tparam K a type representing a continuation
  */
trait IStore[F[_], C, P, A, K] {

  implicit val syncF: Sync[F]

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

  def toMap: F[Map[Seq[C], Row[P, A, K]]]

  private[rspace] def close: F[Unit]

  private[rspace] val trieStore: ITrieStore[F, TrieTransaction, Blake2b256Hash, GNAT[C, P, A, K]]

  private[rspace] val trieBranch: Branch

  def withTrieTxn[R](txn: Transaction)(f: TrieTransaction => R): F[R]

  private val _trieUpdates: AtomicAny[(Long, List[TrieUpdate[C, P, A, K]])] =
    AtomicAny[(Long, List[TrieUpdate[C, P, A, K]])]((0L, Nil))

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  def trieDelete(key: Blake2b256Hash, gnat: GNAT[C, P, A, K]): F[Unit] =
    syncF.delay {
      _trieUpdates.getAndTransform {
        case (count, list) =>
          (count + 1, TrieUpdate(count, Delete, key, gnat) :: list)
      }
    }

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  def trieInsert(key: Blake2b256Hash, gnat: GNAT[C, P, A, K]): F[Unit] =
    syncF.delay {
      _trieUpdates.getAndTransform {
        case (count, list) =>
          (count + 1, TrieUpdate(count, Insert, key, gnat) :: list)
      }
    }

  private[rspace] def getTrieUpdates: Seq[TrieUpdate[C, P, A, K]] =
    _trieUpdates.get._2

  private[rspace] def getTrieUpdateCount: Long =
    _trieUpdates.get._1

  protected def processTrieUpdate(update: TrieUpdate[C, P, A, K]): F[Unit]

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  private[rspace] def getAndClearTrieUpdates(): Seq[TrieUpdate[C, P, A, K]] =
    _trieUpdates.getAndTransform(kp((0L, Nil)))._2

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  // TODO stop throwing exceptions
  def createCheckpoint(): F[Blake2b256Hash] =
    for {
      trieUpdates <- syncF.delay(getAndClearTrieUpdates())
      _ <- Traverse[Seq].sequence_ {
            collapse(trieUpdates).map(processTrieUpdate)
          }
      checkPoint <- syncF.delay {
                     trieStore.withTxn(trieStore.createTxnWrite()) { txn =>
                       trieStore
                         .persistAndGetRoot(txn, trieBranch)
                         .getOrElse(throw new Exception("Could not get root hash"))
                     }
                   }
    } yield checkPoint

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

  def isEmpty: F[Boolean]
}
