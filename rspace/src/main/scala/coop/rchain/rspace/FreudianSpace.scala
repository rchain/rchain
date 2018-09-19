package coop.rchain.rspace

import cats.{Eval, Id, Monoid}
import cats.implicits._
import coop.rchain.catscontrib._
import coop.rchain.rspace.Match.MatchResult
import coop.rchain.rspace.Match.MatchResult.{Error, Found, NotFound}
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
  * @tparam E a type representing an illegal state in matching algorithm
  * @tparam A a type representing an arbitrary piece of data
  * @tparam R a type representing a match result
  * @tparam K a type representing a continuation
  */
trait FreudianSpace[C, P, E, A, S, R, K] extends ISpace[Id, C, P, E, A, S, R, K] {

  /**
    * A store which satisfies the [[IStore]] interface.
    */
  val store: IStore[C, P, A, K]

  val branch: Branch

  protected[this] val eventLog: SyncVar[Log] =
    SyncVarOps.create[Log](Seq.empty)

  /* Consume */

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
  )(implicit m: Match[P, E, A, S, R],
    monoid: Monoid[S]): Eval[MatchResult[(DataCandidate[C, R], Seq[(Datum[A], Int)]), S, E]] =
    data match {
      case Nil => Eval.later(NotFound(monoid.empty))
      case (indexedDatum @ (Datum(matchCandidate, persist, produceRef), dataIndex)) :: remaining =>
        m.get(pattern, matchCandidate) match {
          case Error(s, e) => Eval.later(Error(s, e))
          case Found(s, mat) if persist =>
            Eval.later(
              Found(s, (DataCandidate(channel, Datum(mat, persist, produceRef), dataIndex), data)))
          case Found(s, mat) =>
            Eval.later(
              Found(s,
                    (DataCandidate(channel, Datum(mat, persist, produceRef), dataIndex),
                     prefix ++ remaining)))
          case NotFound(s) =>
            findMatchingDataCandidate(channel, remaining, pattern, indexedDatum +: prefix)
              .map(_.map(s2 => monoid.combine(s, s2)))
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
    * remaining matches. If an illegal state is reached when searching a matching candidate
    * we treat it as if no match was found and append the illegal state to result list.
    */
  private[rspace] final def extractDataCandidates(
      channelPatternPairs: Seq[(C, P)],
      channelToIndexedData: Map[C, Seq[(Datum[A], Int)]],
      acc: Seq[MatchResult[DataCandidate[C, R], S, E]])(
      implicit m: Match[P, E, A, S, R],
      monoid: Monoid[S]): Eval[Seq[MatchResult[DataCandidate[C, R], S, E]]] =
    channelPatternPairs match {
      case Nil =>
        Eval.later(acc.reverse)
      case (channel, pattern) :: tail =>
        val maybeTuple: Eval[MatchResult[(DataCandidate[C, R], Seq[(Datum[A], Int)]), S, E]] =
          channelToIndexedData.get(channel) match {
            case Some(indexedData) =>
              findMatchingDataCandidate(channel, indexedData, pattern, Nil)
            case None =>
              Eval.later(NotFound(monoid.empty))
          }

        maybeTuple.flatMap {
          case Error(s, e) =>
            Eval.later((Error[DataCandidate[C, R], S, E](s, e) +: acc).reverse)
          case Found(s, (cand, rem)) =>
            extractDataCandidates(tail,
                                  channelToIndexedData.updated(channel, rem),
                                  Found[DataCandidate[C, R], S, E](s, cand) +: acc)
          case NotFound(s) =>
            extractDataCandidates(tail,
                                  channelToIndexedData,
                                  NotFound[DataCandidate[C, R], S, E](s) +: acc)
        }
    }

  /* Produce */

  private[rspace] final def extractFirstMatch(
      channels: Seq[C],
      matchCandidates: Seq[(WaitingContinuation[P, K], Int)],
      channelToIndexedData: Map[C, Seq[(Datum[A], Int)]])(
      implicit m: Match[P, E, A, S, R],
      monoid: Monoid[S]): Eval[MatchResult[ProduceCandidate[C, P, R, K], S, E]] =
    matchCandidates match {
      case Nil =>
        Eval.later(NotFound(monoid.empty))
      case (p @ WaitingContinuation(patterns, _, _, _), index) :: remaining =>
        val maybeDataCandidates: Eval[MatchResult[Seq[DataCandidate[C, R]], S, E]] =
          extractDataCandidates(channels.zip(patterns), channelToIndexedData, Nil).map(seq => {
            if (seq.isEmpty)
              MatchResult.NotFound(monoid.empty)
            else {
              val init: MatchResult[Seq[DataCandidate[C, R]], S, E] =
                Found(monoid.empty, Seq.empty[DataCandidate[C, R]])
              seq.foldRight(init) {
                case (curr, acc) =>
                  curr match {
                    case NotFound(s1) =>
                      acc.fold(
                        (s2, _) => NotFound(monoid.combine(s1, s2)),
                        s2 => NotFound(monoid.combine(s1, s2)),
                        (s2, e) => Error(monoid.combine(s1, s2), e)
                      )
                    case Found(s1, v) =>
                      acc.fold(
                        (s2, d) => Found(monoid.combine(s1, s2), v +: d),
                        s2 => NotFound(monoid.combine(s1, s2)),
                        (s2, e) => Error(monoid.combine(s1, s2), e)
                      )
                    case Error(s1, e) =>
                      acc.fold(
                        (s2, _) => Error(monoid.combine(s1, s2), e),
                        s2 => Error(monoid.combine(s1, s2), e),
                        (s2, _) => Error(monoid.combine(s1, s2), e)
                      )
                  }
              }
            }
          })
        maybeDataCandidates.flatMap {
          case Error(s, e) => Eval.later(Error(s, e))
          case NotFound(s1) =>
            extractFirstMatch(channels, remaining, channelToIndexedData).map(_.map(s2 =>
              monoid.combine(s1, s2)))
          case Found(s, dataCandidates) =>
            Eval.later(Found(s, ProduceCandidate(channels, p, index, dataCandidates)))
        }
    }

  override def close(): Id[Unit] = store.close()
}
