package coop.rchain.rspace

import java.nio.file.Path

import cats.Applicative
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.implicits._
import com.google.common.collect.Multiset
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.rspace.history.{Branch, HistoryRepository}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.{Produce, _}
import coop.rchain.shared.Log
import monix.execution.atomic.AtomicAny

import scala.collection.JavaConverters._
import scala.collection.SortedSet
import scala.concurrent.ExecutionContext

class ReplayRSpace[F[_]: Sync, C, P, A, R, K](
    historyRepository: HistoryRepository[F, C, P, A, K],
    storeAtom: AtomicAny[HotStore[F, C, P, A, K]],
    branch: Branch
)(
    implicit
    serializeC: Serialize[C],
    serializeP: Serialize[P],
    serializeA: Serialize[A],
    serializeK: Serialize[K],
    val concurrent: Concurrent[F],
    protected val logF: Log[F],
    contextShift: ContextShift[F],
    scheduler: ExecutionContext,
    metricsF: Metrics[F],
    val spanF: Span[F]
) extends RSpaceOps[F, C, P, A, R, K](historyRepository, storeAtom, branch)
    with IReplaySpace[F, C, P, A, R, K] {

  protected[this] override val logger: Logger = Logger[this.type]

  implicit protected[this] lazy val MetricsSource: Metrics.Source =
    Metrics.Source(RSpaceMetricsSource, "replay")

  def store: HotStore[F, C, P, A, K] = storeAtom.get()

  private[this] val consumeCommLabel = "comm.consume"
  private[this] val produceCommLabel = "comm.produce"
  private[this] val consumeSpanLabel = Metrics.Source(MetricsSource, "consume")
  private[this] val produceSpanLabel = Metrics.Source(MetricsSource, "produce")

  def consume(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      sequenceNumber: Int,
      peeks: SortedSet[Int] = SortedSet.empty
  )(
      implicit m: Match[F, P, A, R]
  ): F[MaybeActionResult] =
    contextShift.evalOn(scheduler) {
      if (channels.length =!= patterns.length) {
        val msg = "channels.length must equal patterns.length"
        logF.error(msg) >> syncF.raiseError(new IllegalArgumentException(msg))
      } else
        spanF.trace(consumeSpanLabel) {
          for {
            _ <- spanF.mark("before-consume-lock")
            result <- consumeLockF(channels) {
                       lockedConsume(
                         channels,
                         patterns,
                         continuation,
                         persist,
                         sequenceNumber,
                         peeks
                       )
                     }
            _ <- spanF.mark("post-consume-lock")
          } yield result
        }
    }

  private[this] def lockedConsume(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      sequenceNumber: Int,
      peeks: SortedSet[Int]
  )(
      implicit m: Match[F, P, A, R]
  ): F[MaybeActionResult] = {
    def runMatcher(comm: COMM): F[Option[Seq[DataCandidate[C, R]]]] =
      for {
        channelToIndexedDataList <- channels.traverse { c: C =>
                                     store
                                       .getData(c)
                                       .map(_.zipWithIndex.filter {
                                         case (Datum(_, _, source), _) =>
                                           comm.produces.contains(source)
                                       })
                                       .map(v => c -> v)
                                   }
        result <- extractDataCandidates(channels.zip(patterns), channelToIndexedDataList.toMap, Nil)
                   .map(_.sequence)
      } yield result

    def storeWaitingContinuation(
        consumeRef: Consume,
        maybeCommRef: Option[COMM],
        peeks: SortedSet[Int]
    ): F[MaybeActionResult] =
      for {
        _ <- store.putContinuation(
              channels,
              WaitingContinuation(patterns, continuation, persist, peeks, consumeRef)
            )
        _ <- channels.traverse(channel => store.putJoin(channel, channels))
        _ <- logF.debug(s"""|consume: no data found,
                            |storing <(patterns, continuation): ($patterns, $continuation)>
                            |at <channels: $channels>""".stripMargin.replace('\n', ' '))
      } yield None

    def handleMatches(
        mats: Seq[DataCandidate[C, R]],
        consumeRef: Consume,
        comms: Multiset[COMM],
        peeks: SortedSet[Int],
        channelsToIndex: Map[C, Int]
    ): F[MaybeActionResult] = {
      def shouldRemove(persist: Boolean, channel: C): Boolean =
        !persist && !peeks.contains(channelsToIndex(channel))
      for {
        _       <- metricsF.incrementCounter(consumeCommLabel)
        commRef <- syncF.delay { COMM(consumeRef, mats.map(_.datum.source), peeks) }
        _       <- assertF(comms.contains(commRef), "COMM Event was not contained in the trace")
        r <- mats.toList
              .sortBy(_.datumIndex)(Ordering[Int].reverse)
              .traverse {
                case DataCandidate(candidateChannel, Datum(_, persistData, _), dataIndex) =>
                  if (shouldRemove(persistData, candidateChannel)) {
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
                val contSequenceNumber = commRef.nextSequenceNumber
                Some(
                  (
                    ContResult(
                      continuation,
                      persist,
                      channels,
                      patterns,
                      contSequenceNumber,
                      peeks.nonEmpty
                    ),
                    mats.map(dc => Result(dc.datum.a, dc.datum.persist))
                  )
                )
              }
      } yield r
    }

    def getCommOrDataCandidates(comms: Seq[COMM]): F[Either[COMM, Seq[DataCandidate[C, R]]]] = {
      type COMMOrData = Either[COMM, Seq[DataCandidate[C, R]]]
      def go(comms: Seq[COMM]): F[Either[Seq[COMM], Either[COMM, Seq[DataCandidate[C, R]]]]] =
        comms match {
          case Nil =>
            val msg = "List comms must not be empty"
            logger.error(msg)
            Sync[F].raiseError(new IllegalArgumentException(msg))
          case commRef :: Nil =>
            runMatcher(commRef).map {
              case Some(x) => x.asRight[COMM].asRight[Seq[COMM]]
              case None    => commRef.asLeft[Seq[DataCandidate[C, R]]].asRight[Seq[COMM]]
            }
          case commRef :: rem =>
            runMatcher(commRef).map {
              case Some(x) => x.asRight[COMM].asRight[Seq[COMM]]
              case None    => rem.asLeft[COMMOrData]
            }
        }
      comms.tailRecM(go)
    }

    for {
      _ <- logF.debug(s"""|consume: searching for data matching <patterns: $patterns>
                          |at <channels: $channels>""".stripMargin.replace('\n', ' '))
      consumeRef <- syncF.delay {
                     Consume.create(channels, patterns, continuation, persist, sequenceNumber)
                   }
      _ <- spanF.mark("after-compute-consumeref")
      r <- replayData.get(consumeRef) match {
            case None =>
              storeWaitingContinuation(consumeRef, None, peeks)
            case Some(comms) =>
              val commOrDataCandidates: F[Either[COMM, Seq[DataCandidate[C, R]]]] =
                getCommOrDataCandidates(comms.iterator().asScala.toList)

              val x: F[MaybeActionResult] = commOrDataCandidates.flatMap {
                case Left(commRef) =>
                  storeWaitingContinuation(consumeRef, Some(commRef), peeks)
                case Right(dataCandidates) =>
                  val channelsToIndex = channels.zipWithIndex.toMap
                  handleMatches(
                    dataCandidates,
                    consumeRef,
                    comms,
                    peeks,
                    channelsToIndex
                  )
              }
              x
          }
    } yield r
  }

  def produce(channel: C, data: A, persist: Boolean, sequenceNumber: Int)(
      implicit m: Match[F, P, A, R]
  ): F[MaybeActionResult] =
    contextShift.evalOn(scheduler) {
      spanF.trace(produceSpanLabel) {
        for {
          _ <- spanF.mark("before-produce-lock")
          result <- produceLockF(channel) {
                     lockedProduce(channel, data, persist, sequenceNumber)
                   }
          _ <- spanF.mark("post-produce-lock")
        } yield result
      }
    }

  private[this] def lockedProduce(
      channel: C,
      data: A,
      persist: Boolean,
      sequenceNumber: Int
  )(
      implicit m: Match[F, P, A, R]
  ): F[MaybeActionResult] = {

    type MaybeProduceCandidate = Option[ProduceCandidate[C, P, R, K]]

    def runMatcher(
        comm: COMM,
        produceRef: Produce,
        groupedChannels: Seq[Seq[C]]
    ): F[MaybeProduceCandidate] = {

      def go(groupedChannels: Seq[Seq[C]]): F[Either[Seq[Seq[C]], MaybeProduceCandidate]] =
        groupedChannels match {
          case Nil => none[ProduceCandidate[C, P, R, K]].asRight[Seq[Seq[C]]].pure[F]
          case channels :: remaining =>
            for {
              continuations <- store.getContinuations(channels)
              matchCandidates = continuations.zipWithIndex.filter {
                case (WaitingContinuation(_, _, _, _, source), _) =>
                  comm.consume == source
              }
              channelToIndexedDataList <- channels.traverse { c: C =>
                                           (for {
                                             data <- store.getData(c)
                                           } yield (data.zipWithIndex.filter {
                                             case (Datum(_, _, source), _) =>
                                               comm.produces.contains(source)
                                           })).map(
                                             as =>
                                               c -> {
                                                 if (c == channel)
                                                   Seq((Datum(data, persist, produceRef), -1))
                                                 else as
                                               }
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
    ): F[Either[COMM, ProduceCandidate[C, P, R, K]]] = {
      type COMMOrProduce = Either[COMM, ProduceCandidate[C, P, R, K]]
      def go(comms: Seq[COMM]): F[Either[Seq[COMM], COMMOrProduce]] =
        comms match {
          case Nil =>
            val msg = "comms must not be empty"
            logger.error(msg)
            Sync[F].raiseError(new IllegalArgumentException(msg))
          case commRef :: Nil =>
            runMatcher(commRef, produceRef, groupedChannels) map {
              case Some(x) => x.asRight[COMM].asRight[Seq[COMM]]
              case None    => commRef.asLeft[ProduceCandidate[C, P, R, K]].asRight[Seq[COMM]]
            }
          case commRef :: rem =>
            runMatcher(commRef, produceRef, groupedChannels) map {
              case Some(x) => x.asRight[COMM].asRight[Seq[COMM]]
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
        mat: ProduceCandidate[C, P, R, K],
        produceRef: Produce,
        comms: Multiset[COMM],
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
                        COMM(consumeRef, dataCandidates.map(_.datum.source), peeks)
                      }
            _ <- assertF(comms.contains(commRef), "COMM Event was not contained in the trace")
            _ <- if (!persistK) {
                  store.removeContinuation(channels, continuationIndex)
                } else {
                  ().pure[F]
                }
            _ <- dataCandidates.toList
                  .sortBy(_.datumIndex)(Ordering[Int].reverse)
                  .traverse {
                    case DataCandidate(candidateChannel, Datum(_, persistData, _), dataIndex) =>
                      def shouldRemove: Boolean = {
                        val idx = channelsToIndex(candidateChannel)
                        dataIndex >= 0 && (!persistData && !peeks.contains(idx))
                      }
                      (if (shouldRemove) {
                         store.removeDatum(candidateChannel, dataIndex)
                       } else Applicative[F].unit) >>
                        store.removeJoin(candidateChannel, channels)
                  }
            _ <- logF.debug(s"produce: matching continuation found at <channels: $channels>")
            _ <- removeBindingsFor(commRef)
            r <- syncF.delay {
                  val contSequenceNumber = commRef.nextSequenceNumber
                  Some(
                    (
                      ContResult(
                        continuation,
                        persistK,
                        channels,
                        patterns,
                        contSequenceNumber,
                        peeks.nonEmpty
                      ),
                      dataCandidates.map(dc => Result(dc.datum.a, dc.datum.persist))
                    )
                  )
                }
          } yield r
      }

    for {
      groupedChannels <- store.getJoins(channel)
      _               <- spanF.mark("after-fetch-joins")
      _ <- logF.debug(
            s"""|produce: searching for matching continuations
                |at <groupedChannels: $groupedChannels>""".stripMargin
              .replace('\n', ' ')
          )
      produceRef <- syncF.delay { Produce.create(channel, data, persist, sequenceNumber) }
      _          <- spanF.mark("after-compute-produceref")
      result <- replayData.get(produceRef) match {
                 case None =>
                   storeDatum(produceRef, None)
                 case Some(comms) =>
                   val commOrProduceCandidate: F[Either[COMM, ProduceCandidate[C, P, R, K]]] =
                     getCommOrProduceCandidate(
                       comms.iterator().asScala.toList,
                       produceRef,
                       groupedChannels
                     )
                   val r: F[MaybeActionResult] = commOrProduceCandidate.flatMap {
                     case Left(comm) =>
                       storeDatum(produceRef, Some(comm))
                     case Right(produceCandidate) =>
                       val indexedChannels = produceCandidate.channels.zipWithIndex.toMap
                       for {
                         a <- handleMatch(produceCandidate, produceRef, comms, indexedChannels)
                         _ <- if (produceCandidate.continuation.peeks.contains(
                                    indexedChannels(channel)
                                  )) {
                               storeDatum(produceRef, None)
                             } else
                               ().pure[F]
                       } yield a
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
}

object ReplayRSpace {

  def create[F[_], C, P, A, R, K](
      historyRepository: HistoryRepository[F, C, P, A, K],
      store: HotStore[F, C, P, A, K],
      branch: Branch
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      concurrent: Concurrent[F],
      logF: Log[F],
      contextShift: ContextShift[F],
      scheduler: ExecutionContext,
      metricsF: Metrics[F],
      spanF: Span[F]
  ): F[ReplayRSpace[F, C, P, A, R, K]] = {

    val space: ReplayRSpace[F, C, P, A, R, K] =
      new ReplayRSpace[F, C, P, A, R, K](historyRepository, AtomicAny(store), branch)

    space.pure[F]
  }

  def create[F[_], C, P, A, R, K](
      dataDir: Path,
      mapSize: Long,
      branch: Branch
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      concurrent: Concurrent[F],
      logF: Log[F],
      contextShift: ContextShift[F],
      scheduler: ExecutionContext,
      metricsF: Metrics[F],
      spanF: Span[F]
  ): F[IReplaySpace[F, C, P, A, R, K]] =
    RSpace.setUp[F, C, P, A, R, K](dataDir, mapSize, branch).map {
      case (historyReader, store) =>
        new ReplayRSpace[F, C, P, A, R, K](historyReader, AtomicAny(store), branch)
    }
}
