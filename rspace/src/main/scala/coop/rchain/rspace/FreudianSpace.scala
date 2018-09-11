package coop.rchain.rspace

import cats.Id
import cats.implicits._
import coop.rchain.catscontrib._
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.Log
import coop.rchain.shared.SyncVarOps

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.concurrent.SyncVar

/** The interface for RSpace
  * It's called a Freudian space because it has a fixed Id
  *
  * @tparam C a type representing a channel
  * @tparam P a type representing a pattern
  * @tparam A a type representing an arbitrary piece of data
  * @tparam R a type representing a match result
  * @tparam K a type representing a continuation
  */
trait FreudianSpace[C, P, A, R, K] extends ISpace[Id, C, P, A, R, K] {

  /**
    * A store which statisfies the [[IStore]] interface.
    */
  val store: IStore[C, P, A, K]

  val branch: Branch

  protected[this] val eventLog: SyncVar[Log] =
    SyncVarOps.create[Log](Seq.empty)

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

  override def close(): Id[Unit] = store.close()
}
