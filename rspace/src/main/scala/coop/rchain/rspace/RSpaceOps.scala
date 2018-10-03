package coop.rchain.rspace

import scala.Function.const
import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.concurrent.SyncVar
import scala.util.Random

import cats.effect.Sync
import cats.implicits._
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib._
import coop.rchain.rspace.history.{Branch, Leaf}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.{Consume, Produce}
import coop.rchain.shared.SyncVarOps._
import kamon._
import kamon.trace.Tracer.SpanBuilder

abstract class RSpaceOps[F[_], C, P, E, A, R, K](val store: IStore[C, P, A, K], val branch: Branch)(
    implicit
    serializeC: Serialize[C],
    serializeP: Serialize[P],
    serializeA: Serialize[A],
    serializeK: Serialize[K],
    syncF: Sync[F]
) extends SpaceMatcher[F, C, P, E, A, R, K] {

  private[this] val invalidInstallMsg = "Installing can never trigger a COMM event"

  protected[this] val logger: Logger
  protected[this] val installSpan: SpanBuilder

  private[this] val installs: SyncVar[Installs[C, P, E, A, R, K]] = {
    val installs = new SyncVar[Installs[C, P, E, A, R, K]]()
    installs.put(List.empty)
    installs
  }

  protected[this] def restoreInstalls(txn: store.Transaction): Unit =
    installs.get.foreach {
      case (Install.Consume(channels, patterns, continuation, _match)) =>
        install(txn, channels, patterns, continuation, update = false)(_match)
      case (Install.Produce(channel, data, persist, _match)) =>
        install(txn, channel, data, persist, update = false)(_match)
    }

  private[this] def install(
      txn: store.Transaction,
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      update: Boolean
  )(implicit m: Match[P, E, A, R]): Option[(K, Seq[R])] = {
    if (channels.length =!= patterns.length) {
      val msg = "channels.length must equal patterns.length"
      logger.error(msg)
      throw new IllegalArgumentException(msg)
    }
    logger.debug(s"""|install: searching for data matching <patterns: $patterns>
                     |at <channels: $channels>""".stripMargin.replace('\n', ' '))

    val consumeRef = Consume.create(channels, patterns, continuation, true)

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
        throw new RuntimeException(s"Installing can never result in an invalid match: $e")
      case Right(None) =>
        if (update)
          installs.update(curr => Install.Consume(channels, patterns, continuation, m) :: curr)
        store.installWaitingContinuation(
          txn,
          channels,
          WaitingContinuation(patterns, continuation, persist = true, consumeRef)
        )
        for (channel <- channels) store.addJoin(txn, channel, channels)
        logger.debug(s"""|storing <(patterns, continuation): ($patterns, $continuation)>
                         |at <channels: $channels>""".stripMargin.replace('\n', ' '))
        None
      case Right(Some(_)) =>
        throw new RuntimeException(invalidInstallMsg)
    }

  }

  override def install(channels: Seq[C], patterns: Seq[P], continuation: K)(
      implicit m: Match[P, E, A, R]
  ): F[Option[(K, Seq[R])]] = syncF.delay {
    Kamon.withSpan(installSpan.start(), finishSpan = true) {
      store.withTxn(store.createTxnWrite()) { txn =>
        install(txn, channels, patterns, continuation, true)
      }
    }
  }

  private[this] def install(
      txn: store.Transaction,
      channel: C,
      data: A,
      persist: Boolean,
      update: Boolean
  )(
      implicit m: Match[P, E, A, R]
  ): Option[(K, Seq[R])] = {

    val span = Kamon.currentSpan()
    val groupedChannels: Seq[Seq[C]] =
      store.getJoin(txn, channel)
    span.mark("grouped-channels")
    logger.debug(s"""|produce: searching for matching continuations
                         |at <groupedChannels: $groupedChannels>""".stripMargin.replace('\n', ' '))

    val produceRef = Produce.create(channel, data, persist)

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
          span.mark("before-channel-to-indexed-data")
          val channelToIndexedData: Map[C, Seq[(Datum[A], Int)]] = channels.map { (c: C) =>
            val as =
              Random.shuffle(store.getData(txn, Seq(c)).zipWithIndex)
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
      case Left(e) =>
        throw new RuntimeException(s"Installing can never result in an invalid match: $e")
      case Right(
          Some(_)
          ) =>
        throw new RuntimeException(invalidInstallMsg)
      case Right(None) =>
        logger.debug(s"produce: no matching continuation found")
        if (update)
          installs.update(
            curr => Install.Produce[C, P, E, A, R, K](channel, data, persist, m) :: curr
          )
        span.mark("before-install-datum")
        store.installDatum(txn, Seq(channel), Datum(data, persist, produceRef))
        span.mark("after-install-datum")
        logger.debug(s"produce: persisted <data: $data> at <channel: $channel>")
        None
    }
  }

  override def install(channel: C, data: A, persist: Boolean)(
      implicit m: Match[P, E, A, R]
  ): F[Option[(K, Seq[R])]] =
    syncF.delay {
      Kamon.withSpan(installSpan.start(), finishSpan = true) {
        store.withTxn(store.createTxnWrite()) { txn =>
          install(txn, channel, data, persist, true)
        }
      }
    }

  override def retrieve(
      root: Blake2b256Hash,
      channelsHash: Blake2b256Hash
  ): F[Option[GNAT[C, P, A, K]]] =
    syncF.delay { history.lookup(store.trieStore, root, channelsHash) }

  override def reset(root: Blake2b256Hash): F[Unit] =
    syncF.delay {
      store.withTxn(store.createTxnWrite()) { txn =>
        store.withTrieTxn(txn) { trieTxn =>
          store.trieStore.validateAndPutRoot(trieTxn, store.trieBranch, root)
          val leaves = store.trieStore.getLeaves(trieTxn, root)
          eventLog.update(const(Seq.empty))
          store.clearTrieUpdates()
          store.clear(txn)
          restoreInstalls(txn)
          store.bulkInsert(txn, leaves.map { case Leaf(k, v) => (k, v) })
        }
      }
    }

  override def clear(): F[Unit] = {
    val emptyRootHash: F[Blake2b256Hash] = syncF.delay {
      store.withTxn(store.createTxnRead()) { txn =>
        store.withTrieTxn(txn) { trieTxn =>
          store.trieStore.getEmptyRoot(trieTxn)
        }
      }
    }
    emptyRootHash.flatMap(root => reset(root))
  }
}
