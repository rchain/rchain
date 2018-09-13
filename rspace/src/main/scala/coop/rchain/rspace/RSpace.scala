package coop.rchain.rspace

import cats.implicits._
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib._
import coop.rchain.rspace.concurrent.{DefaultMultiLock, DefaultTwoStepLock, TwoStepLock}
import coop.rchain.rspace.history.{Branch, ITrieStore, InMemoryTrieStore}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.{COMM, Consume, Log, Produce}
import coop.rchain.shared.SyncVarOps._
import scodec.Codec

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.util.Random
import kamon._

class RSpace[C, P, E, A, R, K](store: IStore[C, P, A, K], branch: Branch)(
    implicit
    serializeC: Serialize[C],
    serializeP: Serialize[P],
    serializeA: Serialize[A],
    serializeK: Serialize[K]
) extends RSpaceOps[C, P, E, A, R, K](store, branch) {

  implicit val codecC = serializeC.toCodec

  implicit val channelOrdering: Ordering[Blake2b256Hash] =
    (x: Blake2b256Hash, y: Blake2b256Hash) => {
      x.bytes.toHex.compare(y.bytes.toHex)
    }

  private val lock: TwoStepLock[Blake2b256Hash] = new DefaultTwoStepLock()

  private def consumeLock(channels: Seq[C])(
      thunk: => Either[E, Option[(K, Seq[R])]]): Either[E, Option[(K, Seq[R])]] = {
    val hashes = channels.map(ch => StableHashProvider.hash(ch))
    lock.acquire(hashes)(() => hashes)(thunk)
  }

  private def produceLock(channel: C)(
      thunk: => Either[E, Option[(K, Seq[R])]]): Either[E, Option[(K, Seq[R])]] =
    lock.acquire(Seq(StableHashProvider.hash(channel)))(() =>
      store.withTxn(store.createTxnRead()) { txn =>
        val groupedChannels: Seq[Seq[C]] = store.getJoin(txn, channel)
        groupedChannels.flatten.map(StableHashProvider.hash(_))
    })(thunk)

  override protected[this] val logger: Logger = Logger[this.type]

  private[this] val consumeCommCounter = Kamon.counter("rspace.comm.consume")
  private[this] val produceCommCounter = Kamon.counter("rspace.comm.produce")

  private[this] val consumeSpan   = Kamon.buildSpan("rspace.consume")
  private[this] val produceSpan   = Kamon.buildSpan("rspace.produce")
  protected[this] val installSpan = Kamon.buildSpan("rspace.install")

  override def consume(channels: Seq[C], patterns: Seq[P], continuation: K, persist: Boolean)(
      implicit m: Match[P, E, A, R]): Either[E, Option[(K, Seq[R])]] =
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

      consumeLock(channels) {
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

        val channelToIndexedData = store.withTxn(store.createTxnRead()) { txn =>
          channels.map { c: C =>
            c -> Random.shuffle(store.getData(txn, Seq(c)).zipWithIndex)
          }.toMap
        }

        val options: Either[E, Option[Seq[DataCandidate[C, R]]]] =
          extractDataCandidates(channels.zip(patterns), channelToIndexedData, Nil).sequence
            .map(_.sequence)

        options match {
          case Left(e) =>
            Left(e)
          case Right(None) =>
            store.withTxn(store.createTxnWrite()) { txn =>
              store.putWaitingContinuation(
                txn,
                channels,
                WaitingContinuation(patterns, continuation, persist, consumeRef))
              for (channel <- channels) store.addJoin(txn, channel, channels)
            }
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
                  store.withTxn(store.createTxnWrite()) { txn =>
                    store.removeDatum(txn, Seq(candidateChannel), dataIndex)
                  }
                case _ =>
                  ()
              }
            logger.debug(s"consume: data found for <patterns: $patterns> at <channels: $channels>")
            Right(Some((continuation, dataCandidates.map(_.datum.a))))
        }
      }
    }

  override def produce(channel: C, data: A, persist: Boolean)(
      implicit m: Match[P, E, A, R]): Either[E, Option[(K, Seq[R])]] =
    Kamon.withSpan(produceSpan.start(), finishSpan = true) {
      produceLock(channel) {
        //TODO fix double join fetch
        val groupedChannels: Seq[Seq[C]] = store.withTxn(store.createTxnRead()) { txn =>
          store.getJoin(txn, channel)
        }
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
            data: Datum[A]): Either[E, Option[ProduceCandidate[C, P, R, K]]] =
          groupedChannels match {
            case Nil => Right(None)
            case channels :: remaining =>
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

        extractProduceCandidate(groupedChannels, channel, Datum(data, persist, produceRef)) match {
          case Left(e) => Left(e)
          case Right(
              Some(
                ProduceCandidate(channels,
                                 WaitingContinuation(_, continuation, persistK, consumeRef),
                                 continuationIndex,
                                 dataCandidates))) =>
            produceCommCounter.increment()

            eventLog.update(COMM(consumeRef, dataCandidates.map(_.datum.source)) +: _)

            if (!persistK) {
              store.withTxn(store.createTxnWrite()) { txn =>
                store.removeWaitingContinuation(txn, channels, continuationIndex)
              }
            }
            dataCandidates
              .sortBy(_.datumIndex)(Ordering[Int].reverse)
              .foreach {
                case DataCandidate(candidateChannel, Datum(_, persistData, _), dataIndex) =>
                  if (!persistData && dataIndex >= 0) {
                    store.withTxn(store.createTxnWrite()) { txn =>
                      store.removeDatum(txn, Seq(candidateChannel), dataIndex)
                    }
                  }
                  store.withTxn(store.createTxnWrite()) { txn =>
                    store.removeJoin(txn, candidateChannel, channels)
                  }
              }
            logger.debug(s"produce: matching continuation found at <channels: $channels>")
            Right(Some(continuation, dataCandidates.map(_.datum.a)))
          case Right(None) =>
            logger.debug(s"produce: no matching continuation found")
            store.withTxn(store.createTxnWrite()) { txn =>
              store.putDatum(txn, Seq(channel), Datum(data, persist, produceRef))
            }
            logger.debug(s"produce: persisted <data: $data> at <channel: $channel>")
            Right(None)
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

  def create[C, P, E, A, R, K](context: Context[C, P, A, K], branch: Branch)(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K]): RSpace[C, P, E, A, R, K] =
    create(context.createStore(branch), branch)

  def createInMemory[C, P, E, A, R, K](
      trieStore: ITrieStore[InMemTransaction[history.State[Blake2b256Hash, GNAT[C, P, A, K]]],
                            Blake2b256Hash,
                            GNAT[C, P, A, K]],
      branch: Branch)(implicit
                      sc: Serialize[C],
                      sp: Serialize[P],
                      sa: Serialize[A],
                      sk: Serialize[K]): RSpace[C, P, E, A, R, K] = {

    val mainStore = InMemoryStore
      .create[InMemTransaction[history.State[Blake2b256Hash, GNAT[C, P, A, K]]], C, P, A, K](
        trieStore,
        branch)
    create(mainStore, branch)
  }

  def create[C, P, E, A, R, K](store: IStore[C, P, A, K], branch: Branch)(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K]): RSpace[C, P, E, A, R, K] = {

    implicit val codecC: Codec[C] = sc.toCodec
    implicit val codecP: Codec[P] = sp.toCodec
    implicit val codecA: Codec[A] = sa.toCodec
    implicit val codecK: Codec[K] = sk.toCodec

    val space = new RSpace[C, P, E, A, R, K](store, branch)

    /*
     * history.initialize returns true if the history trie contains no root (i.e. is empty).
     *
     * In this case, we create a checkpoint for the empty store so that we can reset
     * to the empty store state with the clear method.
     */
    val _ = if (history.initialize(store.trieStore, branch)) {
      space.createCheckpoint()
    }

    space
  }
}
