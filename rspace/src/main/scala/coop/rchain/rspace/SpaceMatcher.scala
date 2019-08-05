package coop.rchain.rspace

import cats.effect.Sync
import cats.implicits._
import coop.rchain.catscontrib._
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.Span.TraceId
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

  private[this] val findSpanLabel         = "find-matching-data"
  private[this] val extractSpanLabel      = "extract-matching-data"
  private[this] val extractFirstSpanLabel = "extract-first-matching-data"

  implicit val syncF: Sync[F]
  implicit val spanF: Span[F]

  /* Consume */

  type MatchingDataCandidate = (DataCandidate[C, A], Seq[(Datum[A], Int)])

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
      implicit m: Match[F, P, A],
      traceId: TraceId
  ): F[Option[MatchingDataCandidate]] =
    for {
      _ <- spanF.mark(findSpanLabel)
      res <- data match {
              case (indexedDatum @ (Datum(matchCandidate, persist, produceRef), dataIndex)) +: remaining =>
                m.get(pattern, matchCandidate).flatMap {
                  case None =>
                    findMatchingDataCandidate(channel, remaining, pattern, indexedDatum +: prefix)
                  case Some(mat) if persist =>
                    (DataCandidate(channel, Datum(mat, persist, produceRef), dataIndex), data).some
                      .pure[F]
                  case Some(mat) =>
                    (
                      DataCandidate(channel, Datum(mat, persist, produceRef), dataIndex),
                      prefix ++ remaining
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
      acc: Seq[Option[DataCandidate[C, A]]]
  )(implicit m: Match[F, P, A], traceId: TraceId): F[Seq[Option[DataCandidate[C, A]]]] =
    for {
      _ <- spanF.mark(extractSpanLabel)
      res <- channelPatternPairs match {
              case (channel, pattern) +: tail =>
                for {
                  maybeTuple <- channelToIndexedData.get(channel) match {
                                 case Some(indexedData) =>
                                   findMatchingDataCandidate(channel, indexedData, pattern, Nil)
                                 case None =>
                                   none[(DataCandidate[C, A], Seq[(Datum[A], Int)])].pure[F]
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
  )(implicit m: Match[F, P, A], traceId: TraceId): F[Option[ProduceCandidate[C, P, A, K]]] =
    for {
      _ <- spanF.mark(extractFirstSpanLabel)
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
