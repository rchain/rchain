package coop.rchain.rspace.nextgenrspace

import cats.effect.{Concurrent, ContextShift, Sync}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib._
import coop.rchain.metrics.Metrics
import coop.rchain.metrics.Metrics.Source
import coop.rchain.rspace._
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.internal._
import coop.rchain.rspace.nextgenrspace.history.{HistoryRepository, HistoryRepositoryInstances}
import coop.rchain.rspace.trace._
import coop.rchain.shared.{Cell, Log}
import coop.rchain.shared.SyncVarOps._
import monix.execution.atomic.AtomicAny

import scala.concurrent.ExecutionContext
import scala.util.Random

class RSpace[F[_], C, P, A, R, K] private[rspace] (
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
    logF: Log[F],
    contextShift: ContextShift[F],
    scheduler: ExecutionContext,
    metricsF: Metrics[F]
) extends RSpaceOps[F, C, P, A, R, K](historyRepository, storeAtom, branch)
    with ISpace[F, C, P, A, R, K] {

  def store: HotStore[F, C, P, A, K] = storeAtom.get()

  protected[this] override val logger: Logger = Logger[this.type]

  implicit private[this] val MetricsSource: Source = RSpaceMetricsSource
  private[this] val consumeCommLabel               = "comm.consume"
  private[this] val produceCommLabel               = "comm.produce"
  private[this] val consumeSpanLabel               = Metrics.Source(MetricsSource, "consume")
  private[this] val produceSpanLabel               = Metrics.Source(MetricsSource, "produce")

  /*
   * Here, we create a cache of the data at each channel as `channelToIndexedData`
   * which is used for finding matches.  When a speculative match is found, we can
   * remove the matching datum from the remaining data candidates in the cache.
   *
   * Put another way, this allows us to speculatively remove matching data without
   * affecting the actual store contents.
   */
  private[this] def fetchChannelToIndexData(channels: Seq[C]): F[Map[C, Seq[(Datum[A], Int)]]] =
    channels
      .traverse { c: C =>
        for {
          data <- store.getData(c)
        } yield c -> Random.shuffle(data.zipWithIndex)
      }
      .map(_.toMap)

  private[this] def storeWaitingContinuation(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      consumeRef: Consume
  ): F[MaybeActionResult] =
    for {
      _ <- store.putContinuation(
            channels,
            WaitingContinuation(
              patterns,
              continuation,
              persist,
              consumeRef
            )
          )
      _ <- channels.traverse { channel =>
            store.putJoin(channel, channels)
          }
      _ <- logF.debug(s"""|consume: no data found,
                          |storing <(patterns, continuation): ($patterns, $continuation)>
                          |at <channels: $channels>""".stripMargin.replace('\n', ' '))
    } yield None

  private[this] def storePersistentData(dataCandidates: Seq[DataCandidate[C, R]]): F[List[Unit]] =
    dataCandidates.toList
      .sortBy(_.datumIndex)(Ordering[Int].reverse)
      .traverse {
        case DataCandidate(
            candidateChannel,
            Datum(_, persistData, _),
            dataIndex
            ) if !persistData =>
          store.removeDatum(
            candidateChannel,
            dataIndex
          )
        case _ =>
          ().pure[F]
      }

  private[this] def createContinuationResult(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      consumeRef: Consume,
      dataCandidates: Seq[DataCandidate[C, R]]
  ): MaybeActionResult = {
    val contSequenceNumber: Int =
      nextSequenceNumber(consumeRef, dataCandidates)
    Some(
      (
        ContResult(
          continuation,
          persist,
          channels,
          patterns,
          contSequenceNumber
        ),
        dataCandidates
          .map(dc => Result(dc.datum.a, dc.datum.persist))
      )
    )
  }

  override def consume(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      sequenceNumber: Int
  )(
      implicit m: Match[F, P, A, R]
  ): F[MaybeActionResult] = {

    def storeWC(consumeRef: Consume): F[MaybeActionResult] =
      storeWaitingContinuation(channels, patterns, continuation, persist, consumeRef)

    def wrapResult(
        consumeRef: Consume,
        dataCandidates: Seq[DataCandidate[C, R]]
    ): MaybeActionResult =
      createContinuationResult(
        channels,
        patterns,
        continuation,
        persist,
        consumeRef,
        dataCandidates
      )

    contextShift.evalOn(scheduler) {
      if (channels.isEmpty) {
        val msg = "channels can't be empty"
        logF.error(msg) *> syncF.raiseError(new IllegalArgumentException(msg))
      } else if (channels.length =!= patterns.length) {
        val msg = "channels.length must equal patterns.length"
        logF.error(msg) *> syncF.raiseError(new IllegalArgumentException(msg))
      } else
        for {
          span <- metricsF.span(consumeSpanLabel)
          _    <- span.mark("before-consume-ref-compute")
          consumeRef <- syncF.delay {
                         Consume.create(channels, patterns, continuation, persist, sequenceNumber)
                       }
          result <- consumeLockF(channels) {
                     for {
                       _ <- span.mark("consume-lock-acquired")
                       _ <- logF
                             .debug(
                               s"""|consume: searching for data matching <patterns: $patterns>
                                   |at <channels: $channels>""".stripMargin.replace('\n', ' ')
                             )
                       _ <- syncF.delay { eventLog.update(consumeRef +: _) }
                       _ <- span.mark("event-log-updated")

                       channelToIndexedData <- fetchChannelToIndexData(channels)
                       _                    <- span.mark("channel-to-indexed-data-fetched")

                       options <- extractDataCandidates(
                                   channels.zip(patterns),
                                   channelToIndexedData,
                                   Nil
                                 ).map(_.sequence)
                       _ <- span.mark("extract-consume-candidate")
                       result <- options match {
                                  case None => storeWC(consumeRef)
                                  case Some(dataCandidates) =>
                                    for {
                                      _ <- metricsF.incrementCounter(consumeCommLabel)
                                      _ <- syncF.delay {
                                            eventLog.update(
                                              COMM(consumeRef, dataCandidates.map(_.datum.source)) +: _
                                            )
                                          }
                                      _ <- storePersistentData(dataCandidates)
                                      _ <- logF.debug(
                                            s"consume: data found for <patterns: $patterns> at <channels: $channels>"
                                          )
                                    } yield wrapResult(consumeRef, dataCandidates)

                                }
                       _ <- span.mark("extract-consume-candidate")
                     } yield result

                   }
          _ <- span.mark("post-consume-lock")
          _ <- span.close()
        } yield result
    }
  }

  private[this] def nextSequenceNumber(
      consumeRef: Consume,
      dataCandidates: Seq[DataCandidate[C, R]]
  ): Int =
    Math.max(
      consumeRef.sequenceNumber,
      dataCandidates.map {
        case DataCandidate(_, Datum(_, _, source), _) => source.sequenceNumber
      }.max
    ) + 1

  /*
   * Find produce candidate
   */

  type MaybeProduceCandidate = Option[ProduceCandidate[C, P, R, K]]

  type CandidateChannels = Seq[C]

  private[this] def extractProduceCandidate(
      groupedChannels: Seq[CandidateChannels],
      batChannel: C,
      data: Datum[A]
  )(implicit m: Match[F, P, A, R]): F[MaybeProduceCandidate] = {

    def go(
        acc: Seq[CandidateChannels]
    ): F[Either[Seq[CandidateChannels], MaybeProduceCandidate]] =
      acc match {
        case Nil =>
          none[ProduceCandidate[C, P, R, K]].asRight[Seq[CandidateChannels]].pure[F]
        case channels :: remaining =>
          for {
            matchCandidates <- for {
                                data <- store.getContinuations(channels)
                              } yield (Random.shuffle(data.zipWithIndex))

            /*
             * Here, we create a cache of the data at each channel as `channelToIndexedData`
             * which is used for finding matches.  When a speculative match is found, we can
             * remove the matching datum from the remaining data candidates in the cache.
             *
             * Put another way, this allows us to speculatively remove matching data without
             * affecting the actual store contents.
             *
             * In this version, we also add the produced data directly to this cache.
             */
            channelToIndexedDataList <- channels.traverse { c: C =>
                                         (for {
                                           data <- store.getData(c)
                                         } yield (Random.shuffle(data.zipWithIndex)))
                                           .map(
                                             as =>
                                               c -> {
                                                 if (c == batChannel)
                                                   (data, -1) +: as
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
            case produceCandidate => produceCandidate.asRight[Seq[CandidateChannels]]
          }
      }
    groupedChannels.tailRecM(go)
  }

  private[this] def processMatchFound(
      pc: ProduceCandidate[C, P, R, K]
  ): F[MaybeActionResult] =
    pc match {
      case ProduceCandidate(
          channels,
          WaitingContinuation(
            patterns,
            continuation,
            persistK,
            consumeRef
          ),
          continuationIndex,
          dataCandidates
          ) =>
        def registerCOMM: F[Unit] =
          syncF.delay {
            eventLog
              .update(
                COMM(consumeRef, dataCandidates.map(_.datum.source)) +: _
              )
          }

        def maybePersistWaitingContinuation: F[Unit] =
          if (!persistK) {
            store.removeContinuation(
              channels,
              continuationIndex
            )
          } else ().pure[F]

        def removeMatchedDatumAndJoin: F[Seq[Unit]] =
          dataCandidates
            .sortBy(_.datumIndex)(Ordering[Int].reverse)
            .traverse {
              case DataCandidate(
                  candidateChannel,
                  Datum(_, persistData, _),
                  dataIndex
                  ) => {
                (if (!persistData && dataIndex >= 0) {
                   store.removeDatum(
                     candidateChannel,
                     dataIndex
                   )
                 } else ().pure[F]) *> store.removeJoin(candidateChannel, channels)
              }
            }

        def constructResult: MaybeActionResult = {
          val contSequenceNumber = nextSequenceNumber(consumeRef, dataCandidates)
          Some(
            (
              ContResult[C, P, K](
                continuation,
                persistK,
                channels,
                patterns,
                contSequenceNumber
              ),
              dataCandidates.map(dc => Result(dc.datum.a, dc.datum.persist))
            )
          )
        }

        for {
          _ <- metricsF.incrementCounter(produceCommLabel)
          _ <- registerCOMM
          _ <- maybePersistWaitingContinuation
          _ <- removeMatchedDatumAndJoin
          _ <- logF.debug(
                s"produce: matching continuation found at <channels: $channels>"
              )
        } yield constructResult
    }

  private[this] def storeData(
      channel: C,
      data: A,
      persist: Boolean,
      produceRef: Produce
  ): F[MaybeActionResult] =
    for {
      _ <- logF.debug(s"produce: no matching continuation found")
      _ <- store.putDatum(channel, Datum(data, persist, produceRef))
      _ <- logF.debug(s"produce: persisted <data: $data> at <channel: $channel>")
    } yield None

  override def produce(channel: C, data: A, persist: Boolean, sequenceNumber: Int)(
      implicit m: Match[F, P, A, R]
  ): F[MaybeActionResult] =
    contextShift.evalOn(scheduler) {
      for {
        span       <- metricsF.span(produceSpanLabel)
        _          <- span.mark("before-produce-ref-computed")
        produceRef <- syncF.delay { Produce.create(channel, data, persist, sequenceNumber) }
        _          <- span.mark("before-produce-lock")
        result <- produceLockF(channel) {
                   for {
                     _ <- span.mark("produce-lock-acquired")
                     //TODO fix double join fetch
                     groupedChannels <- store.getJoins(channel)
                     _               <- span.mark("grouped-channels")
                     _ <- logF.debug(
                           s"""|produce: searching for matching continuations
                               |at <groupedChannels: $groupedChannels>""".stripMargin
                             .replace('\n', ' ')
                         )
                     _ <- syncF.delay { eventLog.update(produceRef +: _) }
                     _ <- span.mark("event-log-updated")
                     extracted <- extractProduceCandidate(
                                   groupedChannels,
                                   channel,
                                   Datum(data, persist, produceRef)
                                 )
                     _ <- span.mark("extract-produce-candidate")
                     r <- extracted match {
                           case Some(pc) => processMatchFound(pc)
                           case None =>
                             storeData(channel, data, persist, produceRef)
                         }
                     _ <- span.mark("process-matching")
                   } yield r
                 }
        _ <- span.mark("post-produce-lock")
        _ <- span.close()
      } yield result
    }

  override def createCheckpoint(): F[Checkpoint] =
    for {
      changes     <- storeAtom.get().changes()
      nextHistory <- historyRepositoryAtom.get().checkpoint(changes.toList)
      _           <- createNewHotStore(nextHistory)(serializeP.toCodec, serializeK.toCodec)
      log         = eventLog.take()
      _           = eventLog.put(Seq.empty)
    } yield Checkpoint(nextHistory.history.root, log)

  override def reset(root: Blake2b256Hash): F[Unit] =
    for {
      nextHistory <- historyRepositoryAtom.get().reset(root)
      _           = historyRepositoryAtom.set(nextHistory)
      _           = eventLog.take()
      _           = eventLog.put(Seq.empty)
      _           <- createNewHotStore(nextHistory)(serializeP.toCodec, serializeK.toCodec)
      _           <- restoreInstalls()
    } yield ()

  protected[rspace] override def isDirty(root: Blake2b256Hash): F[Boolean] = ???
}

object RSpace {

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
      metricsF: Metrics[F]
  ): F[ISpace[F, C, P, A, R, K]] = {
    val space: ISpace[F, C, P, A, R, K] =
      new RSpace[F, C, P, A, R, K](historyRepository, AtomicAny(store), branch)

    space.pure[F]

  }
}
