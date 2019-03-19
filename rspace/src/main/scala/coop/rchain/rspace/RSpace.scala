package coop.rchain.rspace

import java.nio.ByteBuffer

import cats.effect.{Concurrent, ContextShift}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib._
import coop.rchain.metrics.Metrics
import coop.rchain.metrics.Metrics.Source
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace._
import coop.rchain.shared.Log
import coop.rchain.shared.SyncVarOps._
import org.lmdbjava.Txn
import scodec.Codec

import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext
import scala.util.Random

class RSpace[F[_], C, P, A, R, K] private[rspace] (
    store: IStore[F, C, P, A, K],
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
) extends RSpaceOps[F, C, P, A, R, K](store, branch)
    with ISpace[F, C, P, A, R, K] {

  override protected[this] val logger: Logger = Logger[this.type]

  private[this] implicit val MetricsSource: Source = RSpaceMetricsSource
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
    store.withReadTxnF { txn =>
      channels.map { c: C =>
        c -> Random.shuffle(
          store.getData(txn, Seq(c)).zipWithIndex
        )
      }.toMap
    }

  private[this] def storeWaitingContinuation(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      consumeRef: Consume
  ): F[MaybeActionResult] =
    for {
      _ <- store.withWriteTxnF { txn =>
            store.putWaitingContinuation(
              txn,
              channels,
              WaitingContinuation(
                patterns,
                continuation,
                persist,
                consumeRef
              )
            )
            for (channel <- channels)
              store.addJoin(txn, channel, channels)
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
          store.withWriteTxnF { txn =>
            store.removeDatum(
              txn,
              Seq(candidateChannel),
              dataIndex
            )
          }
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
            matchCandidates <- store.withReadTxnF { txn =>
                                Random.shuffle(
                                  store
                                    .getWaitingContinuation(txn, channels)
                                    .zipWithIndex
                                )
                              }
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
                                         store
                                           .withReadTxnF { txn =>
                                             Random.shuffle(
                                               store
                                                 .getData(txn, Seq(c))
                                                 .zipWithIndex
                                             )
                                           }
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
          } yield
            firstMatch match {
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
            store.withWriteTxnF { txn =>
              store.removeWaitingContinuation(
                txn,
                channels,
                continuationIndex
              )
            }
          } else ().pure[F]

        def removeMatchedDatumAndJoin: F[Seq[Unit]] =
          dataCandidates
            .sortBy(_.datumIndex)(Ordering[Int].reverse)
            .traverse {
              case DataCandidate(
                  candidateChannel,
                  Datum(_, persistData, _),
                  dataIndex
                  ) =>
                store.withWriteTxnF { txn =>
                  if (!persistData && dataIndex >= 0) {
                    store.removeDatum(
                      txn,
                      Seq(candidateChannel),
                      dataIndex
                    )
                  }
                  store.removeJoin(txn, candidateChannel, channels)
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
      _ <- store
            .withWriteTxnF { txn =>
              store.putDatum(txn, Seq(channel), Datum(data, persist, produceRef))
            }
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
                     groupedChannels <- store.withReadTxnF {
                                         store.getJoin(_, channel)
                                       }
                     _ <- span.mark("grouped-channels")
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
    contextShift.evalOn(scheduler) {
      syncF.delay {
        val root   = store.createCheckpoint()
        val events = eventLog.take()
        eventLog.put(Seq.empty)
        Checkpoint(root, events)
      }
    }
}

object RSpace {

  def create[F[_], C, P, A, R, K](context: Context[F, C, P, A, K], branch: Branch)(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      concurrent: Concurrent[F],
      log: Log[F],
      contextShift: ContextShift[F],
      scheduler: ExecutionContext,
      metricsF: Metrics[F]
  ): F[ISpace[F, C, P, A, R, K]] = {
    type InMemTXN    = InMemTransaction[history.State[Blake2b256Hash, GNAT[C, P, A, K]]]
    type ByteBuffTXN = Txn[ByteBuffer]

    context match {
      case ctx: LMDBContext[F, C, P, A, K] =>
        create(LMDBStore.create[F, C, P, A, K](ctx, branch), branch)

      case ctx: InMemoryContext[F, C, P, A, K] =>
        create(InMemoryStore.create[F, InMemTXN, C, P, A, K](ctx.trieStore, branch), branch)

      case ctx: MixedContext[F, C, P, A, K] =>
        create(
          LockFreeInMemoryStore.create[F, ByteBuffTXN, C, P, A, K](ctx.trieStore, branch),
          branch
        )
    }
  }

  def create[F[_], C, P, A, R, K](store: IStore[F, C, P, A, K], branch: Branch)(
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

    implicit val codecC: Codec[C] = sc.toCodec
    implicit val codecP: Codec[P] = sp.toCodec
    implicit val codecA: Codec[A] = sa.toCodec
    implicit val codecK: Codec[K] = sk.toCodec

    val space: ISpace[F, C, P, A, R, K] =
      new RSpace[F, C, P, A, R, K](store, branch)

    /*
     * history.initialize returns true if the history trie contains no root (i.e. is empty).
     *
     * In this case, we create a checkpoint for the empty store so that we can reset
     * to the empty store state with the clear method.
     */
    if (history.initialize(store.trieStore, branch)) {
      space.createCheckpoint().map(_ => space)
    } else {
      space.pure[F]
    }
  }
}
