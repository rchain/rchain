package coop.rchain.rspace

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext
import scala.util.Random

import cats.effect.{ContextShift, Sync}
import cats.implicits._
import coop.rchain.shared.Log
import coop.rchain.catscontrib._
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace._
import coop.rchain.shared.SyncVarOps._

import com.typesafe.scalalogging.Logger
import kamon._
import scodec.Codec

class RSpace[F[_], C, P, E, A, R, K] private[rspace] (
    store: IStore[C, P, A, K],
    branch: Branch
)(
    implicit
    serializeC: Serialize[C],
    serializeP: Serialize[P],
    serializeA: Serialize[A],
    serializeK: Serialize[K],
    val syncF: Sync[F],
    logF: Log[F],
    contextShift: ContextShift[F],
    scheduler: ExecutionContext
) extends RSpaceOps[F, C, P, E, A, R, K](store, branch)
    with ISpace[F, C, P, E, A, R, K] {

  override protected[this] val logger: Logger = Logger[this.type]

  private[this] val MetricsSource      = RSpaceMetricsSource
  private[this] val consumeCommCounter = Kamon.counter(MetricsSource + ".comm.consume")
  private[this] val produceCommCounter = Kamon.counter(MetricsSource + ".comm.produce")

  private[this] val consumeSpan   = Kamon.buildSpan(MetricsSource + ".consume")
  private[this] val produceSpan   = Kamon.buildSpan(MetricsSource + ".produce")
  protected[this] val installSpan = Kamon.buildSpan(MetricsSource + ".install")

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements")) // TODO remove when Kamon replaced with Metrics API
  override def consume(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      sequenceNumber: Int
  )(
      implicit m: Match[P, E, A, R]
  ): F[Either[E, Option[(ContResult[C, P, K], Seq[Result[R]])]]] =
    contextShift.evalOn(scheduler) {

      if (channels.isEmpty) {
        val msg = "channels can't be empty"
        logF.error(msg) *> syncF.raiseError(new IllegalArgumentException(msg))
      } else if (channels.length =!= patterns.length) {
        val msg = "channels.length must equal patterns.length"
        logF.error(msg) *> syncF.raiseError(new IllegalArgumentException(msg))
      } else
        syncF.delay {
          Kamon.withSpan(consumeSpan.start(), finishSpan = true) {
            val span = Kamon.currentSpan()
            span.mark("before-consume-ref-compute")
            val consumeRef =
              Consume.create(channels, patterns, continuation, persist, sequenceNumber)

            span.mark("before-consume-lock")
            consumeLock(channels) {
              span.mark("consume-lock-acquired")
              logger.debug(s"""|consume: searching for data matching <patterns: $patterns>
                             |at <channels: $channels>""".stripMargin.replace('\n', ' '))

              span.mark("before-event-log-lock-acquired")
              eventLog.update(consumeRef +: _)
              span.mark("event-log-updated")
              /*
               * Here, we create a cache of the data at each channel as `channelToIndexedData`
               * which is used for finding matches.  When a speculative match is found, we can
               * remove the matching datum from the remaining data candidates in the cache.
               *
               * Put another way, this allows us to speculatively remove matching data without
               * affecting the actual store contents.
               */

              span.mark("before-channel-to-indexed-data")
              val channelToIndexedData = store.withTxn(store.createTxnRead()) { txn =>
                channels.map { c: C =>
                  c -> Random.shuffle(store.getData(txn, Seq(c)).zipWithIndex)
                }.toMap
              }

              span.mark("before-extract-data-candidates")
              val options: Either[E, Option[Seq[DataCandidate[C, R]]]] =
                extractDataCandidates(channels.zip(patterns), channelToIndexedData, Nil).sequence
                  .map(_.sequence)

              options match {
                case Left(e) =>
                  Left(e)
                case Right(None) =>
                  span.mark("acquire-write-lock")

                  store
                    .withTxn(store.createTxnWrite()) { txn =>
                      span.mark("before-put-continuation")
                      store.putWaitingContinuation(
                        txn,
                        channels,
                        WaitingContinuation(patterns, continuation, persist, consumeRef)
                      )
                      span.mark("after-put-continuation")
                      for (channel <- channels)
                        store.addJoin(txn, channel, channels)
                    }
                  logger.debug(s"""|consume: no data found,
                                 |storing <(patterns, continuation): ($patterns, $continuation)>
                                 |at <channels: $channels>""".stripMargin.replace('\n', ' '))
                  Right(None)
                case Right(Some(dataCandidates)) =>
                  consumeCommCounter.increment()

                  span.mark("before-event-log-update")
                  eventLog.update(COMM(consumeRef, dataCandidates.map(_.datum.source)) +: _)
                  span.mark("event-log-updated")

                  dataCandidates
                    .sortBy(_.datumIndex)(Ordering[Int].reverse)
                    .foreach {
                      case DataCandidate(candidateChannel, Datum(_, persistData, _), dataIndex)
                          if !persistData =>
                        span.mark("acquire-write-lock")
                        store.withTxn(store.createTxnWrite()) { txn =>
                          span.mark("before-remove-datum")
                          store.removeDatum(txn, Seq(candidateChannel), dataIndex)
                          span.mark("after-remove-datum")
                        }
                      case _ =>
                        ()
                    }
                  logger.debug(
                    s"consume: data found for <patterns: $patterns> at <channels: $channels>"
                  )
                  val contSequenceNumber: Int = nextSequenceNumber(consumeRef, dataCandidates)
                  Right(
                    Some(
                      (
                        ContResult(continuation, persist, channels, patterns, contSequenceNumber),
                        dataCandidates.map(dc => Result(dc.datum.a, dc.datum.persist))
                      )
                    )
                  )

              }
            }
          }
        }
    }

  private def nextSequenceNumber(consumeRef: Consume, dataCandidates: Seq[DataCandidate[C, R]]) =
    Math.max(
      consumeRef.sequenceNumber,
      dataCandidates.map {
        case DataCandidate(_, Datum(_, _, source), _) => source.sequenceNumber
      }.max
    ) + 1

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements")) // TODO remove when Kamon replaced with Metrics API
  override def produce(channel: C, data: A, persist: Boolean, sequenceNumber: Int)(
      implicit m: Match[P, E, A, R]
  ): F[Either[E, Option[(ContResult[C, P, K], Seq[Result[R]])]]] =
    contextShift.evalOn(scheduler) {
      syncF.delay {
        Kamon.withSpan(produceSpan.start(), finishSpan = true) {
          val span = Kamon.currentSpan()
          span.mark("before-produce-ref-computed")
          val produceRef = Produce.create(channel, data, persist, sequenceNumber)
          span.mark("before-produce-lock")
          produceLock(channel) {
            span.mark("produce-lock-acquired")
            //TODO fix double join fetch
            val groupedChannels: Seq[Seq[C]] = store.withTxn(store.createTxnRead()) { txn =>
              store.getJoin(txn, channel)
            }
            span.mark("grouped-channels")
            logger.debug(
              s"""|produce: searching for matching continuations
                  |at <groupedChannels: $groupedChannels>""".stripMargin
                .replace('\n', ' ')
            )
            span.mark("before-event-log-lock-acquired")
            eventLog.update(produceRef +: _)
            span.mark("event-log-updated")

            /*
             * Find produce candidate
             *
             * Could also be implemented with a lazy `foldRight`.
             */
            @tailrec
            def extractProduceCandidate(
                groupedChannels: Seq[Seq[C]],
                batChannel: C,
                data: Datum[A]
            ): Either[E, Option[ProduceCandidate[C, P, R, K]]] =
              groupedChannels match {
                case Nil => Right(None)
                case channels :: remaining =>
                  span.mark("before-match-candidates")
                  val matchCandidates: Seq[(WaitingContinuation[P, K], Int)] =
                    store.withTxn(store.createTxnRead()) { txn =>
                      Random.shuffle(store.getWaitingContinuation(txn, channels).zipWithIndex)
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
                  span.mark("before-channel-to-indexed-data")
                  val channelToIndexedData: Map[C, Seq[(Datum[A], Int)]] = channels.map { (c: C) =>
                    val as = store.withTxn(store.createTxnRead()) { txn =>
                      Random.shuffle(store.getData(txn, Seq(c)).zipWithIndex)
                    }
                    c -> {
                      if (c == batChannel) (data, -1) +: as else as
                    }
                  }.toMap
                  extractFirstMatch(channels, matchCandidates, channelToIndexedData) match {
                    case Left(e)                 => Left(e)
                    case Right(None)             => extractProduceCandidate(remaining, batChannel, data)
                    case Right(produceCandidate) => Right(produceCandidate)
                  }
              }

            span.mark("extract-produce-candidate")
            extractProduceCandidate(groupedChannels, channel, Datum(data, persist, produceRef)) match {
              case Left(e) => Left(e)
              case Right(
                  Some(
                    ProduceCandidate(
                      channels,
                      WaitingContinuation(patterns, continuation, persistK, consumeRef),
                      continuationIndex,
                      dataCandidates
                    )
                  )
                  ) =>
                produceCommCounter.increment()

                eventLog.update(COMM(consumeRef, dataCandidates.map(_.datum.source)) +: _)

                if (!persistK) {
                  span.mark("acquire-write-lock")
                  store.withTxn(store.createTxnWrite()) { txn =>
                    span.mark("before-remove-continuation")
                    store.removeWaitingContinuation(txn, channels, continuationIndex)
                    span.mark("after-remove-continuation")
                  }
                }
                dataCandidates
                  .sortBy(_.datumIndex)(Ordering[Int].reverse)
                  .foreach {
                    case DataCandidate(candidateChannel, Datum(_, persistData, _), dataIndex) =>
                      span.mark("acquire-write-lock")
                      store.withTxn(store.createTxnWrite()) { txn =>
                        if (!persistData && dataIndex >= 0) {
                          span.mark("before-remove-datum")
                          store.removeDatum(txn, Seq(candidateChannel), dataIndex)
                          span.mark("after-remove-datum")
                        }
                        span.mark("before-remove-join")
                        store.removeJoin(txn, candidateChannel, channels)
                        span.mark("remove-join")
                      }
                  }
                logger.debug(s"produce: matching continuation found at <channels: $channels>")
                val contSequenceNumber: Int = nextSequenceNumber(consumeRef, dataCandidates)
                Right(
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
                )
              case Right(None) =>
                logger.debug(s"produce: no matching continuation found")
                span.mark("acquire-write-lock")
                store.withTxn(store.createTxnWrite()) { txn =>
                  span.mark("before-put-datum")
                  store.putDatum(txn, Seq(channel), Datum(data, persist, produceRef))
                  span.mark("after-put-datum")
                }
                logger.debug(s"produce: persisted <data: $data> at <channel: $channel>")
                Right(None)
            }
          }
        }
      }
    }

  override def createCheckpoint(): F[Checkpoint] =
    syncF.delay {
      val root   = store.createCheckpoint()
      val events = eventLog.take()
      eventLog.put(Seq.empty)
      Checkpoint(root, events)
    }
}

object RSpace {

  def create[F[_], C, P, E, A, R, K](context: Context[C, P, A, K], branch: Branch)(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      syncF: Sync[F],
      log: Log[F],
      contextShift: ContextShift[F],
      scheduler: ExecutionContext
  ): F[ISpace[F, C, P, E, A, R, K]] =
    context match {
      case ctx: LMDBContext[C, P, A, K] =>
        create(LMDBStore.create[C, P, A, K](ctx, branch), branch)

      case ctx: InMemoryContext[C, P, A, K] =>
        create(InMemoryStore.create(ctx.trieStore, branch), branch)

      case ctx: MixedContext[C, P, A, K] =>
        create(LockFreeInMemoryStore.create(ctx.trieStore, branch), branch)
    }

  def create[F[_], C, P, E, A, R, K](store: IStore[C, P, A, K], branch: Branch)(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      syncF: Sync[F],
      logF: Log[F],
      contextShift: ContextShift[F],
      scheduler: ExecutionContext
  ): F[ISpace[F, C, P, E, A, R, K]] = {

    implicit val codecC: Codec[C] = sc.toCodec
    implicit val codecP: Codec[P] = sp.toCodec
    implicit val codecA: Codec[A] = sa.toCodec
    implicit val codecK: Codec[K] = sk.toCodec

    val space: ISpace[F, C, P, E, A, R, K] =
      new RSpace[F, C, P, E, A, R, K](store, branch)

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
