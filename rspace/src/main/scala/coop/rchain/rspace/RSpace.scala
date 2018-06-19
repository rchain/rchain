package coop.rchain.rspace

import cats.implicits._
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib._
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.{COMM, Consume, Log, Produce}
import coop.rchain.shared.SyncVarOps._
import scodec.Codec

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.concurrent.SyncVar
import scala.util.Random

class RSpace[C, P, A, K](val store: IStore[C, P, A, K], val branch: Branch)(
    implicit
    serializeC: Serialize[C],
    serializeP: Serialize[P],
    serializeA: Serialize[A],
    serializeK: Serialize[K]
) extends ISpace[C, P, A, K] {

  private val logger: Logger = Logger[this.type]

  private val eventLog: SyncVar[Log] = {
    val log = new SyncVar[Log]()
    log.put(Seq.empty)
    log
  }

  def consume(channels: Seq[C], patterns: Seq[P], continuation: K, persist: Boolean)(
      implicit m: Match[P, A]): Option[(K, Seq[A])] =
    store.eventsCounter.registerConsume {
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

        val options: Option[Seq[DataCandidate[C, A]]] =
          extractDataCandidates(channels.zip(patterns), channelToIndexedData, Nil).sequence

        options match {
          case None =>
            store.putWaitingContinuation(
              txn,
              channels,
              WaitingContinuation(patterns, continuation, persist, consumeRef))
            for (channel <- channels) store.addJoin(txn, channel, channels)
            logger.debug(s"""|consume: no data found,
                             |storing <(patterns, continuation): ($patterns, $continuation)>
                             |at <channels: $channels>""".stripMargin.replace('\n', ' '))
            None
          case Some(dataCandidates) =>
            store.eventsCounter.registerConsumeCommEvent()

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
            Some((continuation, dataCandidates.map(_.datum.a)))
        }
      }
    }

  def install(channels: Seq[C], patterns: Seq[P], continuation: K)(
      implicit m: Match[P, A]): Option[(K, Seq[A])] = {
    if (channels.length =!= patterns.length) {
      val msg = "channels.length must equal patterns.length"
      logger.error(msg)
      throw new IllegalArgumentException(msg)
    }
    store.withTxn(store.createTxnWrite()) { txn =>
      logger.debug(s"""|install: searching for data matching <patterns: $patterns>
                       |at <channels: $channels>""".stripMargin.replace('\n', ' '))

      val consumeRef = Consume.create(channels, patterns, continuation, true)
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

      val options: Option[Seq[DataCandidate[C, A]]] =
        extractDataCandidates(channels.zip(patterns), channelToIndexedData, Nil).sequence

      options match {
        case None =>
          store.removeAll(txn, channels)
          store.putWaitingContinuation(
            txn,
            channels,
            WaitingContinuation(patterns, continuation, persist = true, consumeRef))
          for (channel <- channels) store.addJoin(txn, channel, channels)
          logger.debug(s"""|consume: no data found,
                           |storing <(patterns, continuation): ($patterns, $continuation)>
                           |at <channels: $channels>""".stripMargin.replace('\n', ' '))
          None
        case Some(dataCandidates) =>
          store.eventsCounter.registerInstallCommEvent()

          eventLog.update(COMM(consumeRef, dataCandidates.map(_.datum.source)) +: _)

          dataCandidates.foreach {
            case DataCandidate(candidateChannel, Datum(_, persistData, _), dataIndex)
                if !persistData =>
              store.removeDatum(txn, Seq(candidateChannel), dataIndex)
            case _ =>
              ()
          }
          logger.debug(s"consume: data found for <patterns: $patterns> at <channels: $channels>")
          Some((continuation, dataCandidates.map(_.datum.a)))
      }
    }
  }

  def produce(channel: C, data: A, persist: Boolean)(implicit m: Match[P, A]): Option[(K, Seq[A])] =
    store.eventsCounter.registerProduce {
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
        def extractProduceCandidate(groupedChannels: Seq[Seq[C]],
                                    batChannel: C,
                                    data: Datum[A]): Option[ProduceCandidate[C, P, A, K]] =
          groupedChannels match {
            case Nil => None
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
                case None             => extractProduceCandidate(remaining, batChannel, data)
                case produceCandidate => produceCandidate
              }
          }

        extractProduceCandidate(groupedChannels, channel, Datum(data, persist, produceRef)) match {
          case Some(
              ProduceCandidate(channels,
                               WaitingContinuation(_, continuation, persistK, consumeRef),
                               continuationIndex,
                               dataCandidates)) =>
            store.eventsCounter.registerProduceCommEvent()

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
            Some(continuation, dataCandidates.map(_.datum.a))
          case None =>
            logger.debug(s"produce: no matching continuation found")
            store.putDatum(txn, Seq(channel), Datum(data, persist, produceRef))
            logger.debug(s"produce: persisted <data: $data> at <channel: $channel>")
            None
        }
      }
    }

  def createCheckpoint(): Checkpoint = {
    val root   = store.createCheckpoint()
    val events = eventLog.take()
    eventLog.put(Seq.empty)
    Checkpoint(root, events)
  }
}

object RSpace {

  def create[C, P, A, K](context: Context[C, P, A, K], branch: Branch)(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K]): RSpace[C, P, A, K] = {

    implicit val codecC: Codec[C] = sc.toCodec
    implicit val codecP: Codec[P] = sp.toCodec
    implicit val codecA: Codec[A] = sa.toCodec
    implicit val codecK: Codec[K] = sk.toCodec

    history.initialize(context.trieStore, branch)

    val mainStore = LMDBStore.create[C, P, A, K](context, branch)

    new RSpace[C, P, A, K](mainStore, branch)
  }
}
