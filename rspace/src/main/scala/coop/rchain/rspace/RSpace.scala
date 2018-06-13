package coop.rchain.rspace

import cats.implicits._
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib._
import coop.rchain.rspace.history._
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.{COMM, Consume, Log, Produce}

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.concurrent.SyncVar
import coop.rchain.shared.SyncVarOps._
import scala.util.Random

class RSpace[C, P, A, K](val store: IStore[C, P, A, K])(implicit
                                                        serializeC: Serialize[C],
                                                        serializeP: Serialize[P],
                                                        serializeA: Serialize[A],
                                                        serializeK: Serialize[K])
    extends ISpace[C, P, A, K] {

  private val logger: Logger = Logger[this.type]

  private val eventLog: SyncVar[Log] = {
    val log = new SyncVar[Log]()
    log.put(Seq.empty)
    log
  }

  /* Consume */

  /** Searches through data, looking for a match with a given pattern.
    *
    * If there is a match, we return the matching [[DataCandidate]],
    * along with the remaining unmatched data.
    */
  @tailrec
  private[rspace] final def findMatchingDataCandidate(
      channel: C,
      data: Seq[(Datum[A], Int)],
      pattern: P,
      prefix: Seq[(Datum[A], Int)]
  )(implicit m: Match[P, A]): Option[(DataCandidate[C, A], Seq[(Datum[A], Int)])] =
    data match {
      case Nil => None
      case (indexedDatum @ (Datum(matchCandidate, persist, produceRef), dataIndex)) :: remaining =>
        m.get(pattern, matchCandidate) match {
          case None =>
            findMatchingDataCandidate(channel, remaining, pattern, indexedDatum +: prefix)
          case Some(mat) if persist =>
            Some((DataCandidate(channel, Datum(mat, persist, produceRef), dataIndex), data))
          case Some(mat) =>
            Some(
              (DataCandidate(channel, Datum(mat, persist, produceRef), dataIndex),
               prefix ++ remaining))
        }
    }

  /** Iterates through (channel, pattern) pairs looking for matching data.
    *
    * Potential match candidates are supplied by the `channelToIndexedData` cache.
    *
    * After a match is found, we remove the matching datum from the candidate cache for
    * remaining matches.
    */
  @tailrec
  private[rspace] final def extractDataCandidates(
      channelPatternPairs: Seq[(C, P)],
      channelToIndexedData: Map[C, Seq[(Datum[A], Int)]],
      acc: Seq[Option[DataCandidate[C, A]]])(
      implicit m: Match[P, A]): Seq[Option[DataCandidate[C, A]]] =
    channelPatternPairs match {
      case Nil =>
        acc.reverse
      case (channel, pattern) :: tail =>
        val maybeTuple: Option[(DataCandidate[C, A], Seq[(Datum[A], Int)])] =
          for {
            indexedData <- channelToIndexedData.get(channel)
            result      <- findMatchingDataCandidate(channel, indexedData, pattern, Nil)
          } yield result

        maybeTuple match {
          case Some((cand, rem)) =>
            extractDataCandidates(tail,
                                  channelToIndexedData.updated(channel, rem),
                                  Some(cand) +: acc)
          case None =>
            extractDataCandidates(tail, channelToIndexedData, None +: acc)
        }
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

  /* Produce */

  @tailrec
  private[rspace] final def extractFirstMatch(
      channels: Seq[C],
      matchCandidates: Seq[(WaitingContinuation[P, K], Int)],
      channelToIndexedData: Map[C, Seq[(Datum[A], Int)]])(
      implicit m: Match[P, A]): Option[ProduceCandidate[C, P, A, K]] =
    matchCandidates match {
      case Nil =>
        None
      case (p @ WaitingContinuation(patterns, _, _, _), index) :: remaining =>
        val maybeDataCandidates: Option[Seq[DataCandidate[C, A]]] =
          extractDataCandidates(channels.zip(patterns), channelToIndexedData, Nil).sequence
        maybeDataCandidates match {
          case None =>
            extractFirstMatch(channels, remaining, channelToIndexedData)
          case Some(dataCandidates) =>
            Some(ProduceCandidate(channels, p, index, dataCandidates))
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
                store.getWaitingContinuation(txn, channels).zipWithIndex
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
              extractFirstMatch(channels, Random.shuffle(matchCandidates), channelToIndexedData) match {
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
                case _ =>
                  ()
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

  def getCheckpoint(): Checkpoint = {
    val root = store
      .getCheckpoint()
      .getOrElse(throw new Exception("Couldn't get checkpoint for an empty Trie"))
    val events = eventLog.take()
    eventLog.put(Seq.empty)
    Checkpoint(root, events)
  }

  def reset(): Unit =
    store.withTxn(store.createTxnWrite()) { txn =>
      store.trieStore.putRoot(txn, EmptyPointer)
      store.clear(txn)
    }

  def reset(hash: Blake2b256Hash): Unit =
    store.withTxn(store.createTxnWrite()) { txn =>
      val pointer = store.trieStore.get(txn, hash) match {
        case Some(Node(_))    => NodePointer(hash)
        case Some(Leaf(_, _)) => LeafPointer(hash)
        case None             => throw new Exception(s"Couldn't reset to element with hash $hash")
      }

      store.trieStore.putRoot(txn, pointer)
      val leaves: Seq[Leaf[Blake2b256Hash, GNAT[C, P, A, K]]] =
        store.trieStore.getLeaves(txn, hash)
      store.clear(txn)
      store.bulkInsert(txn, leaves.map { case Leaf(k, v) => (k, v) })
    }

  def close(): Unit = store.close()
}
