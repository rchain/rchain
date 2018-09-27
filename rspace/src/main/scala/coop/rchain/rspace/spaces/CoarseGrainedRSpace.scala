package coop.rchain.rspace.spaces

import cats.effect.Sync
import cats.implicits._
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib._
import coop.rchain.rspace._
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.{COMM, Consume, Produce}
import coop.rchain.shared.SyncVarOps._

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.util.Random
import kamon._

class CoarseGrainedRSpace[F[_], C, P, E, A, R, K] private[rspace] (
    store: IStore[C, P, A, K],
    branch: Branch
)(
    implicit
    serializeC: Serialize[C],
    serializeP: Serialize[P],
    serializeA: Serialize[A],
    serializeK: Serialize[K],
    val syncF: Sync[F]
) extends RSpaceOps[F, C, P, E, A, R, K](store, branch) {

  override protected[this] val logger: Logger = Logger[this.type]

  private[this] val consumeCommCounter = Kamon.counter("rspace.comm.consume")
  private[this] val produceCommCounter = Kamon.counter("rspace.comm.produce")

  private[this] val consumeSpan   = Kamon.buildSpan("rspace.consume")
  private[this] val produceSpan   = Kamon.buildSpan("rspace.produce")
  protected[this] val installSpan = Kamon.buildSpan("rspace.install")

  override def consume(channels: Seq[C], patterns: Seq[P], continuation: K, persist: Boolean)(
      implicit m: Match[P, E, A, R]
  ): F[Either[E, Option[(K, Seq[R])]]] = syncF.delay {
    Kamon.withSpan(consumeSpan.start(), finishSpan = true) {
      if (channels.isEmpty) {
        val msg = "channels can't be empty"
        logger.error(msg)
        throw new IllegalArgumentException(msg)
      }
      if (channels.length =!= patterns.length) {
        val msg = "channels.length must equal patterns.length"
        logger.error(msg)
        throw new IllegalArgumentException(msg)
      }

      store.withTxn(store.createTxnWrite()) { txn =>
        logger.debug(s"""|consume: searching for data matching <patterns: $patterns>
                         |at <channels: $channels>""".stripMargin.replace('\n', ' '))

        val consumeRef = Consume.create(channels, patterns, continuation, persist)
        eventLog.update(consumeRef +: _)

        /*
         * Here, we create a cache of the data at each channel as `channelToIndexedData`
         * which is used for finding matches.  When a speculative match is found, we can
         * remove the matching datum from the remaining data candidates in the cache.
         *
         * Put another way, this allows us to speculatively remove matching data without
         * affecting the actual store contents.
         */

        val channelToIndexedData = channels.map { (c: C) =>
          c -> Random.shuffle(store.getData(txn, Seq(c)).zipWithIndex)
        }.toMap

        val options: Either[E, Option[Seq[DataCandidate[C, R]]]] =
          extractDataCandidates(channels.zip(patterns), channelToIndexedData, Nil).sequence
            .map(_.sequence)

        options match {
          case Left(e) =>
            Left(e)
          case Right(None) =>
            store.putWaitingContinuation(
              txn,
              channels,
              WaitingContinuation(patterns, continuation, persist, consumeRef)
            )
            for (channel <- channels) store.addJoin(txn, channel, channels)
            logger.debug(s"""|consume: no data found,
                             |storing <(patterns, continuation): ($patterns, $continuation)>
                             |at <channels: $channels>""".stripMargin.replace('\n', ' '))
            Right(None)
          case Right(Some(dataCandidates)) =>
            consumeCommCounter.increment()

            eventLog.update(COMM(consumeRef, dataCandidates.map(_.datum.source)) +: _)

            dataCandidates
              .sortBy(_.datumIndex)(Ordering[Int].reverse)
              .foreach {
                case DataCandidate(candidateChannel, Datum(_, persistData, _), dataIndex)
                    if !persistData =>
                  store.removeDatum(txn, Seq(candidateChannel), dataIndex)
                case _ =>
                  ()
              }
            logger.debug(s"consume: data found for <patterns: $patterns> at <channels: $channels>")
            Right(Some((continuation, dataCandidates.map(_.datum.a))))
        }
      }
    }
  }

  override def produce(channel: C, data: A, persist: Boolean)(
      implicit m: Match[P, E, A, R]
  ): F[Either[E, Option[(K, Seq[R])]]] = syncF.delay {
    Kamon.withSpan(produceSpan.start(), finishSpan = true) {
      store.withTxn(store.createTxnWrite()) { txn =>
        val groupedChannels: Seq[Seq[C]] = store.getJoin(txn, channel)
        logger.debug(s"""|produce: searching for matching continuations
                         |at <groupedChannels: $groupedChannels>""".stripMargin.replace('\n', ' '))

        val produceRef = Produce.create(channel, data, persist)
        eventLog.update(produceRef +: _)

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
              val matchCandidates: Seq[(WaitingContinuation[P, K], Int)] =
                Random.shuffle(store.getWaitingContinuation(txn, channels).zipWithIndex)
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
              val channelToIndexedData: Map[C, Seq[(Datum[A], Int)]] = channels.map { (c: C) =>
                val as = Random.shuffle(store.getData(txn, Seq(c)).zipWithIndex)
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

        extractProduceCandidate(groupedChannels, channel, Datum(data, persist, produceRef)) match {
          case Left(e) => Left(e)
          case Right(
              Some(
                ProduceCandidate(
                  channels,
                  WaitingContinuation(_, continuation, persistK, consumeRef),
                  continuationIndex,
                  dataCandidates
                )
              )
              ) =>
            produceCommCounter.increment()

            eventLog.update(COMM(consumeRef, dataCandidates.map(_.datum.source)) +: _)

            if (!persistK) {
              store.removeWaitingContinuation(txn, channels, continuationIndex)
            }
            dataCandidates
              .sortBy(_.datumIndex)(Ordering[Int].reverse)
              .foreach {
                case DataCandidate(candidateChannel, Datum(_, persistData, _), dataIndex) =>
                  if (!persistData && dataIndex >= 0) {
                    store.removeDatum(txn, Seq(candidateChannel), dataIndex)
                  }
                  store.removeJoin(txn, candidateChannel, channels)
              }
            logger.debug(s"produce: matching continuation found at <channels: $channels>")
            Right(Some(continuation, dataCandidates.map(_.datum.a)))
          case Right(None) =>
            logger.debug(s"produce: no matching continuation found")
            store.putDatum(txn, Seq(channel), Datum(data, persist, produceRef))
            logger.debug(s"produce: persisted <data: $data> at <channel: $channel>")
            Right(None)
        }
      }
    }
  }

  def createCheckpoint(): F[Checkpoint] = syncF.delay {
    val root   = store.createCheckpoint()
    val events = eventLog.take()
    eventLog.put(Seq.empty)
    Checkpoint(root, events)
  }
}
