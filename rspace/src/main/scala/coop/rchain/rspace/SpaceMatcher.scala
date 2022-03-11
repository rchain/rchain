package coop.rchain.rspace

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.catscontrib._
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.rspace.internal._

/** The interface for RSpace
  *
  * @tparam C a type representing a channel
  * @tparam P a type representing a pattern
  * @tparam A a type representing an arbitrary piece of data and match result
  * @tparam K a type representing a continuation
  */
private[rspace] trait SpaceMatcher[F[_], C, P, A, K] extends ISpace[F, C, P, A, K] {

  protected[this] def MetricsSource: Source

  def syncF: Sync[F]
  def spanF: Span[F]

  implicit val _syncF = syncF

  /* Consume */

  type MatchingDataCandidate = (ConsumeCandidate[C, A], Seq[(Datum[A], Int)])

  /** Searches through data, looking for a match with a given pattern.
    *
    * If there is a match, we return the matching [[ConsumeCandidate]],
    * along with the remaining unmatched data. If an illegal state is reached
    * during searching for a match we short circuit and return the state.
    */
  private[rspace] final def findMatchingDataCandidate(
      channel: C,
      data: Seq[(Datum[A], Int)],
      pattern: P,
      prefix: Seq[(Datum[A], Int)]
  )(
      implicit m: Match[F, P, A]
  ): F[Option[MatchingDataCandidate]] =
    for {
      res <- data match {
              case (indexedDatum @ (Datum(matchCandidate, persist, produceRef), dataIndex)) +: remaining =>
                m.get(pattern, matchCandidate).flatMap {
                  case None =>
                    findMatchingDataCandidate(channel, remaining, pattern, indexedDatum +: prefix)
                  case Some(mat) =>
                    val indexedDatums = if (persist) data else prefix ++ remaining
                    (
                      ConsumeCandidate(
                        channel,
                        Datum(mat, persist, produceRef),
                        matchCandidate,
                        dataIndex
                      ),
                      indexedDatums
                    ).some.pure[F]
                }
              case _ => none[MatchingDataCandidate].pure[F]
            }
    } yield res

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
      acc: Seq[Option[ConsumeCandidate[C, A]]]
  )(implicit m: Match[F, P, A]): F[Seq[Option[ConsumeCandidate[C, A]]]] =
    for {
      res <- channelPatternPairs match {
              case (channel, pattern) +: tail =>
                for {
                  maybeTuple <- channelToIndexedData.get(channel) match {
                                 case Some(indexedData) =>
                                   findMatchingDataCandidate(channel, indexedData, pattern, Nil)
                                 case None =>
                                   none[(ConsumeCandidate[C, A], Seq[(Datum[A], Int)])].pure[F]
                               }
                  dataCandidates <- maybeTuple match {
                                     case Some((cand, rem)) =>
                                       extractDataCandidates(
                                         tail,
                                         channelToIndexedData.updated(channel, rem),
                                         Some(cand) +: acc
                                       )
                                     case None =>
                                       extractDataCandidates(
                                         tail,
                                         channelToIndexedData,
                                         None +: acc
                                       )
                                   }
                } yield dataCandidates
              case _ => acc.reverse.pure[F]
            }
    } yield res

  /* Produce */

  private[rspace] final def extractFirstMatch(
      channels: Seq[C],
      matchCandidates: Seq[(WaitingContinuation[P, K], Int)],
      channelToIndexedData: Map[C, Seq[(Datum[A], Int)]]
  )(implicit m: Match[F, P, A]): F[Option[ProduceCandidate[C, P, A, K]]] =
    for {
      res <- matchCandidates match {
              case (p @ WaitingContinuation(patterns, _, _, _, _), index) +: remaining =>
                for {
                  maybeDataCandidates <- extractDataCandidates(
                                          channels.zip(patterns),
                                          channelToIndexedData,
                                          Nil
                                        ).map(_.sequence)
                  produceCandidates <- maybeDataCandidates match {
                                        case None =>
                                          extractFirstMatch(
                                            channels,
                                            remaining,
                                            channelToIndexedData
                                          )
                                        case Some(dataCandidates) =>
                                          ProduceCandidate(channels, p, index, dataCandidates).some
                                            .pure[F]
                                      }
                } yield produceCandidates
              case _ => none[ProduceCandidate[C, P, A, K]].pure[F]
            }
    } yield res
}
