package coop.rchain.rspace

import cats.effect.Sync
import cats.implicits._
import cats.syntax._
import coop.rchain.catscontrib._
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
  * @tparam A a type representing an arbitrary piece of data
  * @tparam R a type representing a match result
  * @tparam K a type representing a continuation
  */
private[rspace] trait SpaceMatcher[F[_], C, P, A, R, K] extends ISpace[F, C, P, A, R, K] {

  /**
    * A store which satisfies the [[IStore]] interface.
    */
  val store: IStore[F, C, P, A, K]

  val branch: Branch

  protected[this] val eventLog: SyncVar[Log] =
    SyncVarOps.create[Log](Seq.empty)

  implicit val syncF: Sync[F]

  /* Consume */

  type MatchingDataCandidate = (DataCandidate[C, R], Seq[(Datum[A], Int)])

  /** Searches through data, looking for a match with a given pattern.
    *
    * If there is a match, we return the matching [[DataCandidate]],
    * along with the remaining unmatched data. If an illegal state is reached
    * during searching for a match we short circuit and return the state.
    */
  private[rspace] final def findMatchingDataCandidate(
      channel: C,
      data: Seq[(Datum[A], Int)],
      pattern: P,
      prefix: Seq[(Datum[A], Int)]
  )(
      implicit m: Match[F, P, A, R]
  ): F[Option[MatchingDataCandidate]] =
    data match {
      case (indexedDatum @ (Datum(matchCandidate, persist, produceRef), dataIndex)) +: remaining =>
        m.get(pattern, matchCandidate).flatMap {
          case None =>
            findMatchingDataCandidate(channel, remaining, pattern, indexedDatum +: prefix)
          case Some(mat) if persist =>
            (DataCandidate(channel, Datum(mat, persist, produceRef), dataIndex), data).some.pure[F]
          case Some(mat) =>
            (
              DataCandidate(channel, Datum(mat, persist, produceRef), dataIndex),
              prefix ++ remaining
            ).some.pure[F]
        }
      case _ => none[MatchingDataCandidate].pure[F]
    }

  /** Iterates through (channel, pattern) pairs looking for matching data.
    *
    * Potential match candidates are supplied by the `channelToIndexedData` cache.
    *
    * After a match is found, we remove the matching datum from the candidate cache for
    * remaining matches. If an illegal state is reached when searching a matching candidate
    * we treat it as if no match was found and append the illegal state to result list.
    */
  private[rspace] final def extractDataCandidates(
      channelPatternPairs: Seq[(C, P)],
      channelToIndexedData: Map[C, Seq[(Datum[A], Int)]],
      acc: Seq[Option[DataCandidate[C, R]]]
  )(implicit m: Match[F, P, A, R]): F[Seq[Option[DataCandidate[C, R]]]] =
    channelPatternPairs match {
      case (channel, pattern) +: tail =>
        for {
          maybeTuple <- channelToIndexedData.get(channel) match {
                         case Some(indexedData) =>
                           findMatchingDataCandidate(channel, indexedData, pattern, Nil)
                         case None =>
                           none[(DataCandidate[C, R], Seq[(Datum[A], Int)])].pure[F]
                       }
          dataCandidates <- maybeTuple match {
                             case Some((cand, rem)) =>
                               extractDataCandidates(
                                 tail,
                                 channelToIndexedData.updated(channel, rem),
                                 Some(cand) +: acc
                               )
                             case None =>
                               extractDataCandidates(tail, channelToIndexedData, None +: acc)
                           }
        } yield dataCandidates
      case _ => acc.reverse.pure[F]
    }

  /* Produce */

  private[rspace] final def extractFirstMatch(
      channels: Seq[C],
      matchCandidates: Seq[(WaitingContinuation[P, K], Int)],
      channelToIndexedData: Map[C, Seq[(Datum[A], Int)]]
  )(implicit m: Match[F, P, A, R]): F[Option[ProduceCandidate[C, P, R, K]]] =
    matchCandidates match {
      case (p @ WaitingContinuation(patterns, _, _, _), index) +: remaining =>
        for {
          maybeDataCandidates <- extractDataCandidates(
                                  channels.zip(patterns),
                                  channelToIndexedData,
                                  Nil
                                ).map(_.sequence)
          produceCandidates <- maybeDataCandidates match {
                                case None =>
                                  extractFirstMatch(channels, remaining, channelToIndexedData)
                                case Some(dataCandidates) =>
                                  ProduceCandidate(channels, p, index, dataCandidates).some.pure[F]
                              }
        } yield produceCandidates
      case _ => none[ProduceCandidate[C, P, R, K]].pure[F]
    }

  override def close(): F[Unit] = syncF.delay { store.close() }
}
