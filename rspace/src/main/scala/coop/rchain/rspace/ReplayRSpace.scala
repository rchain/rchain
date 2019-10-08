package coop.rchain.rspace

import java.nio.file.Path

import scala.collection.JavaConverters._
import scala.collection.SortedSet
import scala.concurrent.ExecutionContext

import cats.Applicative
import cats.effect._
import cats.implicits._
import cats.temp.par.Par

import coop.rchain.catscontrib._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.metrics.implicits._
import coop.rchain.rspace.history.{Branch, HistoryRepository}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.{Produce, _}
import coop.rchain.shared.Log
import coop.rchain.shared.SyncVarOps._

import com.google.common.collect.Multiset
import com.typesafe.scalalogging.Logger
import monix.execution.atomic.AtomicAny

class ReplayRSpace[F[_]: Sync, C, P, A, K](
    historyRepository: HistoryRepository[F, C, P, A, K],
    storeAtom: AtomicAny[HotStore[F, C, P, A, K]],
    branch: Branch
)(
    implicit
    serializeC: Serialize[C],
    serializeP: Serialize[P],
    serializeA: Serialize[A],
    serializeK: Serialize[K],
    val m: Match[F, P, A],
    val concurrent: Concurrent[F],
    protected val logF: Log[F],
    contextShift: ContextShift[F],
    scheduler: ExecutionContext,
    metricsF: Metrics[F],
    val spanF: Span[F]
) extends RSpaceOps[F, C, P, A, K](historyRepository, storeAtom, branch)
    with IReplaySpace[F, C, P, A, K] {

  protected[this] override val logger: Logger = Logger[this.type]

  implicit protected[this] lazy val MetricsSource: Metrics.Source =
    Metrics.Source(RSpaceMetricsSource, "replay")

  def store: HotStore[F, C, P, A, K] = storeAtom.get()

  private[this] val consumeCommLabel     = "comm.consume"
  private[this] val consumeTimeCommLabel = "comm.consume-time"
  private[this] val produceCommLabel     = "comm.produce"
  private[this] val produceTimeCommLabel = "comm.produce-time"

  def consume(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      peeks: SortedSet[Int] = SortedSet.empty
  ): F[MaybeActionResult] =
    contextShift.evalOn(scheduler) {
      if (channels.length =!= patterns.length) {
        val msg = "channels.length must equal patterns.length"
        logF.error(msg) >> syncF.raiseError(new IllegalArgumentException(msg))
      } else
        (for {
          result <- consumeLockF(channels) {
                     lockedConsume(
                       channels,
                       patterns,
                       continuation,
                       persist,
                       peeks
                     )
                   }
        } yield result).timer(consumeTimeCommLabel)
    }

  private[this] def lockedConsume(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      peeks: SortedSet[Int]
  ): F[MaybeActionResult] = {
    def runMatcher(comm: COMM): F[Option[Seq[DataCandidate[C, A]]]] =
      for {
        channelToIndexedDataList <- channels.traverse { c: C =>
                                     store
                                       .getData(c)
                                       .map(_.zipWithIndex.filter(matches(comm)))
                                       .map(v => c -> v)
                                   }
        result <- extractDataCandidates(channels.zip(patterns), channelToIndexedDataList.toMap, Nil)
                   .map(_.sequence)
      } yield result

    def handleMatches(
        mats: Seq[DataCandidate[C, A]],
        consumeRef: Consume,
        comms: Multiset[COMM],
        comm: COMM,
        peeks: SortedSet[Int],
        channelsToIndex: Map[C, Int]
    ): F[MaybeActionResult] =
      for {
        _ <- metricsF.incrementCounter(consumeCommLabel)
        commRef <- syncF.delay {
                    COMM(consumeRef, mats.map(_.datum.source), peeks, comm.timesRepeated)
                  }
        _ <- assertF(
              comms.contains(commRef),
              s"COMM Event $commRef was not contained in the trace $comms"
            )
        r <- mats.toList
              .sortBy(_.datumIndex)(Ordering[Int].reverse)
              .traverse {
                case DataCandidate(candidateChannel, Datum(_, persistData, _), _, dataIndex) =>
                  if (!persistData) {
                    store.removeDatum(candidateChannel, dataIndex)
                  } else ().pure[F]
              }
              .flatMap { _ =>
                logF.debug(
                  s"consume: data found for <patterns: $patterns> at <channels: $channels>"
                )
              }
              .flatMap { _ =>
                removeBindingsFor(commRef)
              }
              .map { _ =>
                Some(
                  (
                    ContResult(
                      continuation,
                      persist,
                      channels,
                      patterns,
                      peeks.nonEmpty
                    ),
                    mats
                      .map(dc => Result(dc.channel, dc.datum.a, dc.removedDatum, dc.datum.persist))
                  )
                )
              }
      } yield r

    // TODO: refactor this monster
    def getCommAndDataCandidates(
        comms: Seq[COMM]
    ): F[Option[(COMM, Seq[DataCandidate[C, A]])]] = {
      type COMMOrData = Either[COMM, (COMM, Seq[DataCandidate[C, A]])]
      def go(
          comms: Seq[COMM]
      ): F[Either[Seq[COMM], COMMOrData]] =
        comms match {
          case Nil =>
            val msg = "List comms must not be empty"
            logger.error(msg)
            Sync[F].raiseError(new IllegalArgumentException(msg))
          case commRef :: Nil =>
            runMatcher(commRef).map {
              case Some(dataCandidates) =>
                (commRef, dataCandidates).asRight[COMM].asRight[Seq[COMM]]
              case None => commRef.asLeft[(COMM, Seq[DataCandidate[C, A]])].asRight[Seq[COMM]]
            }
          case commRef :: rem =>
            runMatcher(commRef).map {
              case Some(dataCandidates) =>
                (commRef, dataCandidates).asRight[COMM].asRight[Seq[COMM]]
              case None => rem.asLeft[COMMOrData]
            }
        }
      comms.tailRecM(go).map(_.toOption)
    }

    for {
      _ <- logF.debug(s"""|consume: searching for data matching <patterns: $patterns>
                          |at <channels: $channels>""".stripMargin.replace('\n', ' '))
      consumeRef <- syncF.delay {
                     Consume.create(channels, patterns, continuation, persist)
                   }
      r <- replayData
            .get(consumeRef)
            .fold(
              storeWaitingContinuation(
                channels,
                WaitingContinuation(patterns, continuation, persist, peeks, consumeRef)
              )
            )(
              comms =>
                getCommAndDataCandidates(comms.iterator().asScala.toList).flatMap {
                  _.fold(
                    storeWaitingContinuation(
                      channels,
                      WaitingContinuation(patterns, continuation, persist, peeks, consumeRef)
                    )
                  ) {
                    case (comm, dataCandidates) =>
                      val channelsToIndex = channels.zipWithIndex.toMap
                      handleMatches(
                        dataCandidates,
                        consumeRef,
                        comms,
                        comm,
                        peeks,
                        channelsToIndex
                      )
                  }
                }
            )
    } yield r
  }

  def produce(channel: C, data: A, persist: Boolean): F[MaybeActionResult] =
    contextShift.evalOn(scheduler) {
      (for {
        result <- produceLockF(channel) {
                   lockedProduce(channel, data, persist)
                 }
      } yield result).timer(produceTimeCommLabel)
    }

  private[this] def lockedProduce(
      channel: C,
      data: A,
      persist: Boolean
  ): F[MaybeActionResult] = {

    type MaybeProduceCandidate = Option[ProduceCandidate[C, P, A, K]]

    def runMatcher(
        comm: COMM,
        produceRef: Produce,
        groupedChannels: Seq[Seq[C]]
    ): F[MaybeProduceCandidate] = {

      def go(groupedChannels: Seq[Seq[C]]): F[Either[Seq[Seq[C]], MaybeProduceCandidate]] =
        groupedChannels match {
          case Nil => none[ProduceCandidate[C, P, A, K]].asRight[Seq[Seq[C]]].pure[F]
          case channels :: remaining =>
            for {
              continuations <- store.getContinuations(channels)
              matchCandidates = continuations.zipWithIndex.filter {
                case (WaitingContinuation(_, _, _, _, source), _) =>
                  comm.consume == source
              }
              channelToIndexedDataList <- channels.traverse { c: C =>
                                           store
                                             .getData(c)
                                             .map(_.zipWithIndex)
                                             .map(
                                               as =>
                                                 c -> {
                                                   if (c == channel)
                                                     Seq((Datum(data, persist, produceRef), -1))
                                                   else as
                                                 }.filter { matches(comm) }
                                             )
                                         }
              firstMatch <- extractFirstMatch(
                             channels,
                             matchCandidates,
                             channelToIndexedDataList.toMap
                           )
            } yield firstMatch match {
              case None             => remaining.asLeft[MaybeProduceCandidate]
              case produceCandidate => produceCandidate.asRight[Seq[Seq[C]]]
            }
        }
      groupedChannels.tailRecM(go)
    }

    def getCommOrProduceCandidate(
        comms: Seq[COMM],
        produceRef: Produce,
        groupedChannels: Seq[Seq[C]]
    ): F[Either[COMM, (COMM, ProduceCandidate[C, P, A, K])]] = {
      type COMMOrProduce = Either[COMM, (COMM, ProduceCandidate[C, P, A, K])]
      def go(comms: Seq[COMM]): F[Either[Seq[COMM], COMMOrProduce]] =
        comms match {
          case Nil =>
            val msg = "comms must not be empty"
            logger.error(msg)
            Sync[F].raiseError(new IllegalArgumentException(msg))
          case commRef :: Nil =>
            runMatcher(commRef, produceRef, groupedChannels) map {
              case Some(x) => (commRef, x).asRight[COMM].asRight[Seq[COMM]]
              case None    => commRef.asLeft[(COMM, ProduceCandidate[C, P, A, K])].asRight[Seq[COMM]]
            }
          case commRef :: rem =>
            runMatcher(commRef, produceRef, groupedChannels) map {
              case Some(x) => (commRef, x).asRight[COMM].asRight[Seq[COMM]]
              case None    => rem.asLeft[COMMOrProduce]
            }
        }
      comms.tailRecM(go)
    }

    def storeDatum(
        produceRef: Produce,
        maybeCommRef: Option[COMM]
    ): F[MaybeActionResult] =
      for {
        _ <- store.putDatum(channel, Datum(data, persist, produceRef))
        _ <- logF.debug(s"""|produce: no matching continuation found
                            |storing <data: $data> at <channel: $channel>""".stripMargin)
      } yield None

    def handleMatch(
        mat: ProduceCandidate[C, P, A, K],
        produceRef: Produce,
        comms: Multiset[COMM],
        comm: COMM,
        channelsToIndex: Map[C, Int]
    ): F[MaybeActionResult] =
      mat match {
        case ProduceCandidate(
            channels,
            WaitingContinuation(patterns, continuation, persistK, peeks, consumeRef),
            continuationIndex,
            dataCandidates
            ) =>
          for {
            _ <- metricsF.incrementCounter(produceCommLabel)
            commRef <- syncF.delay {
                        val produceRefs = dataCandidates.map(_.datum.source)
                        COMM(consumeRef, produceRefs, peeks, comm.timesRepeated)
                      }
            _ <- assertF(
                  comms.contains(commRef),
                  s"COMM Event $commRef was not contained in the trace $comms"
                )
            _ <- if (!persistK) {
                  store.removeContinuation(channels, continuationIndex)
                } else {
                  ().pure[F]
                }
            _ <- dataCandidates.toList
                  .sortBy(_.datumIndex)(Ordering[Int].reverse)
                  .traverse {
                    case DataCandidate(candidateChannel, Datum(_, persistData, _), _, dataIndex) =>
                      (if (dataIndex >= 0 && !persistData) {
                         store.removeDatum(candidateChannel, dataIndex)
                       } else Applicative[F].unit) >>
                        store.removeJoin(candidateChannel, channels)
                  }
            _ <- logF.debug(s"produce: matching continuation found at <channels: $channels>")
            _ <- removeBindingsFor(commRef)
            r <- syncF.delay {
                  Some(
                    (
                      ContResult(
                        continuation,
                        persistK,
                        channels,
                        patterns,
                        peeks.nonEmpty
                      ),
                      dataCandidates.map(
                        dc => Result(dc.channel, dc.datum.a, dc.removedDatum, dc.datum.persist)
                      )
                    )
                  )
                }
          } yield r
      }

    for {
      groupedChannels <- store.getJoins(channel)
      _ <- logF.debug(
            s"""|produce: searching for matching continuations
                |at <groupedChannels: $groupedChannels>""".stripMargin
              .replace('\n', ' ')
          )
      produceRef <- syncF.delay { Produce.create(channel, data, persist) }
      _ <- syncF.delay {
            if (!persist)
              produceCounter.update(_.putAndIncrementCounter(produceRef))
          }
      result <- replayData.get(produceRef) match {
                 case None =>
                   storeDatum(produceRef, None)
                 case Some(comms) =>
                   val commOrProduceCandidate
                       : F[Either[COMM, (COMM, ProduceCandidate[C, P, A, K])]] =
                     getCommOrProduceCandidate(
                       comms.iterator().asScala.toList,
                       produceRef,
                       groupedChannels
                     )
                   val r: F[MaybeActionResult] = commOrProduceCandidate.flatMap {
                     case Left(comm) =>
                       storeDatum(produceRef, Some(comm))
                     case Right((comm, produceCandidate)) =>
                       val indexedChannels = produceCandidate.channels.zipWithIndex.toMap
                       handleMatch(produceCandidate, produceRef, comms, comm, indexedChannels)
                   }
                   r
               }
    } yield result
  }

  private def assertF(predicate: Boolean, errorMsg: String): F[Unit] =
    if (!predicate)
      Sync[F].raiseError(
        new IllegalStateException(errorMsg)
      )
    else ().pure[F]

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  private def removeBindingsFor(
      commRef: COMM
  ): F[Unit] = Sync[F].delay {
    commRef.produces.foldLeft(replayData.removeBinding(commRef.consume, commRef)) {
      case (updatedReplays, produceRef) =>
        updatedReplays.removeBinding(produceRef, commRef)
    }
  }

  override def createCheckpoint(): F[Checkpoint] = checkReplayData >> syncF.defer {
    for {
      changes     <- storeAtom.get().changes()
      nextHistory <- historyRepositoryAtom.get().checkpoint(changes.toList)
      _           = historyRepositoryAtom.set(nextHistory)
      _           <- createNewHotStore(nextHistory)(serializeK.toCodec)
      _           <- restoreInstalls()
    } yield (Checkpoint(nextHistory.history.root, Seq.empty))
  }

  override def clear(): F[Unit] = syncF.delay { replayData.clear() } >> super.clear()

  protected[rspace] def isDirty(root: Blake2b256Hash): F[Boolean] = true.pure[F]

  private[this] def matches(comm: COMM)(datumWithIndex: (Datum[A], _)) = {
    val (datum, _) = datumWithIndex
    def wasRepeatedEnoughTimes =
      (if (!datum.persist) {
         comm.timesRepeated(datum.source) === produceCounter.get(
           datum.source
         )
       } else true)
    comm.produces.contains(datum.source) && wasRepeatedEnoughTimes
  }

}

object ReplayRSpace {

  def create[F[_], C, P, A, K](
      historyRepository: HistoryRepository[F, C, P, A, K],
      store: HotStore[F, C, P, A, K],
      branch: Branch
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      m: Match[F, P, A],
      concurrent: Concurrent[F],
      logF: Log[F],
      contextShift: ContextShift[F],
      scheduler: ExecutionContext,
      metricsF: Metrics[F],
      spanF: Span[F]
  ): F[ReplayRSpace[F, C, P, A, K]] = {

    val space: ReplayRSpace[F, C, P, A, K] =
      new ReplayRSpace[F, C, P, A, K](historyRepository, AtomicAny(store), branch)

    space.pure[F]
  }

  def create[F[_], C, P, A, K](
      dataDir: Path,
      mapSize: Long,
      branch: Branch
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      m: Match[F, P, A],
      concurrent: Concurrent[F],
      logF: Log[F],
      contextShift: ContextShift[F],
      scheduler: ExecutionContext,
      metricsF: Metrics[F],
      spanF: Span[F],
      par: Par[F]
  ): F[IReplaySpace[F, C, P, A, K]] =
    RSpace.setUp[F, C, P, A, K](dataDir, mapSize, branch).map {
      case (historyReader, store) =>
        new ReplayRSpace[F, C, P, A, K](historyReader, AtomicAny(store), branch)
    }
}
