package coop.rchain.rspace

import cats.Id
import cats.effect.Sync
import cats.implicits._
import coop.rchain.catscontrib._
import coop.rchain.rspace.ISpace.IdISpace
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.Log
import coop.rchain.shared.SyncVarOps

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.concurrent.SyncVar

/** The interface for RSpace
  *
  * @tparam C a type representing a channel
  * @tparam P a type representing a pattern
  * @tparam E a type representing an illegal state in matching algorithm
  * @tparam A a type representing an arbitrary piece of data
  * @tparam R a type representing a match result
  * @tparam K a type representing a continuation
  */
private[rspace] trait SpaceMatcher[F[_], C, P, E, A, R, K] extends ISpace[F, C, P, E, A, R, K] {

  /**
    * A store which satisfies the [[IStore]] interface.
    */
  val store: IStore[C, P, A, K]

  val branch: Branch

  protected[this] val eventLog: SyncVar[Log] =
    SyncVarOps.create[Log](Seq.empty)

  implicit val syncF: Sync[F]

  /* Consume */

  /** Searches through data, looking for a match with a given pattern.
    *
    * If there is a match, we return the matching [[DataCandidate]],
    * along with the remaining unmatched data. If an illegal state is reached
    * during searching for a match we short circuit and return the state.
    */
  @tailrec
  private[rspace] final def findMatchingDataCandidate(
      channel: C,
      data: Seq[(Datum[A], Int)],
      pattern: P,
      prefix: Seq[(Datum[A], Int)]
  )(implicit m: Match[P, E, A, R]): Either[E, Option[(DataCandidate[C, R], Seq[(Datum[A], Int)])]] =
    data match {
      case Nil => Right(None)
      case (indexedDatum @ (Datum(matchCandidate, persist, produceRef), dataIndex)) :: remaining =>
        m.get(pattern, matchCandidate) match {
          case Left(ex) =>
            Left(ex)
          case Right(None) =>
            findMatchingDataCandidate(channel, remaining, pattern, indexedDatum +: prefix)
          case Right(Some(mat)) if persist =>
            Right(Some((DataCandidate(channel, Datum(mat, persist, produceRef), dataIndex), data)))
          case Right(Some(mat)) =>
            Right(
              Some(
                (
                  DataCandidate(channel, Datum(mat, persist, produceRef), dataIndex),
                  prefix ++ remaining
                )
              )
            )
        }
    }

  def getData(channel: C): F[Seq[Datum[A]]] =
    syncF.delay {
      store.withTxn(store.createTxnRead()) { txn =>
        store.getData(txn, Seq(channel))
      }
    }

  def getWaitingContinuations(channels: Seq[C]): F[Seq[WaitingContinuation[P, K]]] =
    syncF.delay {
      store.withTxn(store.createTxnRead()) { txn =>
        store.getWaitingContinuation(txn, channels)
      }
    }

  /** Iterates through (channel, pattern) pairs looking for matching data.
    *
    * Potential match candidates are supplied by the `channelToIndexedData` cache.
    *
    * After a match is found, we remove the matching datum from the candidate cache for
    * remaining matches. If an illegal state is reached when searching a matching candidate
    * we treat it as if no match was found and append the illegal state to result list.
    */
  @tailrec
  private[rspace] final def extractDataCandidates(
      channelPatternPairs: Seq[(C, P)],
      channelToIndexedData: Map[C, Seq[(Datum[A], Int)]],
      acc: Seq[Either[E, Option[DataCandidate[C, R]]]]
  )(implicit m: Match[P, E, A, R]): Seq[Either[E, Option[DataCandidate[C, R]]]] =
    channelPatternPairs match {
      case Nil =>
        acc.reverse
      case (channel, pattern) :: tail =>
        val maybeTuple: Either[E, Option[(DataCandidate[C, R], Seq[(Datum[A], Int)])]] =
          channelToIndexedData.get(channel) match {
            case Some(indexedData) =>
              findMatchingDataCandidate(channel, indexedData, pattern, Nil)
            case None =>
              Right(None)
          }

        maybeTuple match {
          case Left(e) =>
            (Left(e) +: acc).reverse
          case Right(Some((cand, rem))) =>
            extractDataCandidates(
              tail,
              channelToIndexedData.updated(channel, rem),
              Right(Some(cand)) +: acc
            )
          case Right(None) =>
            extractDataCandidates(tail, channelToIndexedData, Right(None) +: acc)
        }
    }

  /* Produce */

  @tailrec
  private[rspace] final def extractFirstMatch(
      channels: Seq[C],
      matchCandidates: Seq[(WaitingContinuation[P, K], Int)],
      channelToIndexedData: Map[C, Seq[(Datum[A], Int)]]
  )(implicit m: Match[P, E, A, R]): Either[E, Option[ProduceCandidate[C, P, R, K]]] =
    matchCandidates match {
      case Nil =>
        Right(None)
      case (p @ WaitingContinuation(patterns, _, _, _), index) :: remaining =>
        val maybeDataCandidates: Either[E, Option[Seq[DataCandidate[C, R]]]] =
          extractDataCandidates(channels.zip(patterns), channelToIndexedData, Nil).sequence
            .map(_.sequence)
        maybeDataCandidates match {
          case Left(e) => Left(e)
          case Right(None) =>
            extractFirstMatch(channels, remaining, channelToIndexedData)
          case Right(Some(dataCandidates)) =>
            Right(Some(ProduceCandidate(channels, p, index, dataCandidates)))
        }
    }

  override def close(): F[Unit] = syncF.delay { store.close() }
}
