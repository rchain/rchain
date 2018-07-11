package coop.rchain.rspace

import cats.implicits._
import coop.rchain.rspace.history.{Branch, Leaf}
import coop.rchain.catscontrib._
import coop.rchain.rspace.internal._

import scala.annotation.tailrec
import scala.collection.immutable.Seq

/** The interface for RSpace
  *
  * @tparam C a type representing a channel
  * @tparam P a type representing a pattern
  * @tparam A a type representing an arbitrary piece of data
  * @tparam R a type representing a match result
  * @tparam K a type representing a continuation
  */
trait ISpace[C, P, A, R, K] {

  /**
    * A store which statisfies the [[IStore]] interface.
    */
  val store: IStore[C, P, A, K]

  val branch: Branch

  /* Consume */

  /** Searches through data, looking for a match with a given pattern.
    *
    * If there is a match, we return the matching [[DataCandidate]],
    * along with the remaining unmatched data.
    */
  @tailrec
  private[rspace] final def findMatchingDataCandidate(
      channel: C,
      data: Seq[(Datum[A], Int)],
      pattern: P,
      prefix: Seq[(Datum[A], Int)]
  )(implicit m: Match[P, A, R]): Option[(DataCandidate[C, R], Seq[(Datum[A], Int)])] =
    data match {
      case Nil => None
      case (indexedDatum @ (Datum(matchCandidate, persist, produceRef), dataIndex)) :: remaining =>
        m.get(pattern, matchCandidate) match {
          case None =>
            findMatchingDataCandidate(channel, remaining, pattern, indexedDatum +: prefix)
          case Some(mat) if persist =>
            Some((DataCandidate(channel, Datum(mat, persist, produceRef), dataIndex), data))
          case Some(mat) =>
            Some(
              (DataCandidate(channel, Datum(mat, persist, produceRef), dataIndex),
               prefix ++ remaining))
        }
    }

  def getData(channel: C): Seq[Datum[A]] =
    store.withTxn(store.createTxnRead()) { txn =>
      store.getData(txn, Seq(channel))
    }

  def getWaitingContinuations(channels: Seq[C]): Seq[WaitingContinuation[P, K]] =
    store.withTxn(store.createTxnRead()) { txn =>
      store.getWaitingContinuation(txn, channels)
    }

  /** Iterates through (channel, pattern) pairs looking for matching data.
    *
    * Potential match candidates are supplied by the `channelToIndexedData` cache.
    *
    * After a match is found, we remove the matching datum from the candidate cache for
    * remaining matches.
    */
  @tailrec
  private[rspace] final def extractDataCandidates(
      channelPatternPairs: Seq[(C, P)],
      channelToIndexedData: Map[C, Seq[(Datum[A], Int)]],
      acc: Seq[Option[DataCandidate[C, R]]])(
      implicit m: Match[P, A, R]): Seq[Option[DataCandidate[C, R]]] =
    channelPatternPairs match {
      case Nil =>
        acc.reverse
      case (channel, pattern) :: tail =>
        val maybeTuple: Option[(DataCandidate[C, R], Seq[(Datum[A], Int)])] =
          for {
            indexedData <- channelToIndexedData.get(channel)
            result      <- findMatchingDataCandidate(channel, indexedData, pattern, Nil)
          } yield result

        maybeTuple match {
          case Some((cand, rem)) =>
            extractDataCandidates(tail,
                                  channelToIndexedData.updated(channel, rem),
                                  Some(cand) +: acc)
          case None =>
            extractDataCandidates(tail, channelToIndexedData, None +: acc)
        }
    }

  /** Searches the store for data matching all the given patterns at the given channels.
    *
    * If no match is found, then the continuation and patterns are put in the store at the given
    * channels.
    *
    * If a match is found, then the continuation is returned along with the matching data.
    *
    * Matching data stored with the `persist` flag set to `true` will not be removed when it is
    * retrieved. See below for more information about using the `persist` flag.
    *
    * '''NOTE''':
    *
    * A call to [[consume]] that is made with the persist flag set to `true` only persists when
    * there is no matching data.
    *
    * This means that in order to make a continuation "stick" in the store, the user will have to
    * continue to call [[consume]] until a `None` is received.
    *
    * @param channels A Seq of channels on which to search for matching data
    * @param patterns A Seq of patterns with which to search for matching data
    * @param continuation A continuation
    * @param persist Whether or not to attempt to persist the data
    */
  def consume(channels: Seq[C], patterns: Seq[P], continuation: K, persist: Boolean)(
      implicit m: Match[P, A, R]): Option[(K, Seq[R])]

  def install(channels: Seq[C], patterns: Seq[P], continuation: K)(
      implicit m: Match[P, A, R]): Option[(K, Seq[R])]

  /* Produce */

  @tailrec
  private[rspace] final def extractFirstMatch(
      channels: Seq[C],
      matchCandidates: Seq[(WaitingContinuation[P, K], Int)],
      channelToIndexedData: Map[C, Seq[(Datum[A], Int)]])(
      implicit m: Match[P, A, R]): Option[ProduceCandidate[C, P, R, K]] =
    matchCandidates match {
      case Nil =>
        None
      case (p @ WaitingContinuation(patterns, _, _, _), index) :: remaining =>
        val maybeDataCandidates: Option[Seq[DataCandidate[C, R]]] =
          extractDataCandidates(channels.zip(patterns), channelToIndexedData, Nil).sequence
        maybeDataCandidates match {
          case None =>
            extractFirstMatch(channels, remaining, channelToIndexedData)
          case Some(dataCandidates) =>
            Some(ProduceCandidate(channels, p, index, dataCandidates))
        }
    }

  /** Searches the store for a continuation that has patterns that match the given data at the
    * given channel.
    *
    * If no match is found, then the data is put in the store at the given channel.
    *
    * If a match is found, then the continuation is returned along with the matching data.
    *
    * Matching data or continuations stored with the `persist` flag set to `true` will not be
    * removed when they are retrieved. See below for more information about using the `persist`
    * flag.
    *
    * '''NOTE''':
    *
    * A call to [[produce]] that is made with the persist flag set to `true` only persists when
    * there are no matching continuations.
    *
    * This means that in order to make a piece of data "stick" in the store, the user will have to
    * continue to call [[produce]] until a `None` is received.
    *
    * @param channel A channel on which to search for matching continuations and/or store data
    * @param data A piece of data
    * @param persist Whether or not to attempt to persist the data
    */
  def produce(channel: C, data: A, persist: Boolean)(
      implicit m: Match[P, A, R]): Option[(K, Seq[R])]

  /** Creates a checkpoint.
    *
    * @return A [[Checkpoint]]
    */
  def createCheckpoint(): Checkpoint

  /** Resets the store to the given root.
    *
    * @param root A BLAKE2b256 Hash representing the checkpoint
    */
  def reset(root: Blake2b256Hash): Unit =
    store.withTxn(store.createTxnWrite()) { txn =>
      store.withTrieTxn(txn) { trieTxn =>
        store.trieStore.validateAndPutRoot(trieTxn, store.trieBranch, root)
        val leaves = store.trieStore.getLeaves(trieTxn, root)
        store.clear(txn)
        store.bulkInsert(txn, leaves.map { case Leaf(k, v) => (k, v) })
      }
    }

  /** Closes
    */
  def close(): Unit = store.close()
}
