package coop.rchain.rspace

import cats.Id
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib._
import ski._
import coop.rchain.rspace.concurrent.{ConcurrentTwoStepLockF, DefaultTwoStepLock, TwoStepLock}
import coop.rchain.rspace.history.{Branch, Leaf}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.Consume
import coop.rchain.shared.SyncVarOps._

import scala.collection.immutable.Seq
import scala.concurrent.SyncVar
import scala.util.Random

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements")) // TODO remove when Kamon replaced with Metrics API
abstract class RSpaceOps[F[_]: Concurrent, C, P, E, A, R, K](
    val store: IStore[F, C, P, A, K],
    val branch: Branch
)(
    implicit
    serializeC: Serialize[C],
    serializeP: Serialize[P],
    serializeK: Serialize[K]
) extends SpaceMatcher[F, C, P, E, A, R, K] {

  implicit val codecC = serializeC.toCodec

  val syncF: Sync[F] = Concurrent[F]

  private val lockF: TwoStepLock[F, Blake2b256Hash] = new ConcurrentTwoStepLockF()

  protected[this] def consumeLockF(
      channels: Seq[C]
  )(
      thunk: => F[Either[E, Option[(ContResult[C, P, K], Seq[Result[R]])]]]
  ): F[Either[E, Option[(ContResult[C, P, K], Seq[Result[R]])]]] = {
    val hashes = channels.map(ch => StableHashProvider.hash(ch))
    lockF.acquire(hashes)(() => hashes.pure[F])(thunk)
  }

  protected[this] def produceLockF(
      channel: C
  )(
      thunk: => F[Either[E, Option[(ContResult[C, P, K], Seq[Result[R]])]]]
  ): F[Either[E, Option[(ContResult[C, P, K], Seq[Result[R]])]]] =
    lockF.acquire(Seq(StableHashProvider.hash(channel)))(
      () =>
        store.withTxnF(store.createTxnReadF()) { txn =>
          val groupedChannels: Seq[Seq[C]] = store.getJoin(txn, channel)
          groupedChannels.flatten.map(StableHashProvider.hash(_))
        }
    )(thunk)

  protected[this] val logger: Logger

  private[this] val installs: SyncVar[Installs[C, P, E, A, R, K]] = {
    val installs = new SyncVar[Installs[C, P, E, A, R, K]]()
    installs.put(Map.empty)
    installs
  }

  protected[this] def restoreInstalls(txn: store.Transaction): Unit =
    installs.get.foreach {
      case (channels, Install(patterns, continuation, _match)) =>
        install(txn, channels, patterns, continuation)(_match)
    }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  // TODO stop throwing exceptions
  private[this] def install(
      txn: store.Transaction,
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K
  )(implicit m: Match[P, E, A, R]): Option[(K, Seq[R])] = {
    if (channels.length =!= patterns.length) {
      val msg = "channels.length must equal patterns.length"
      logger.error(msg)
      throw new IllegalArgumentException(msg)
    }
    logger.debug(s"""|install: searching for data matching <patterns: $patterns>
                     |at <channels: $channels>""".stripMargin.replace('\n', ' '))

    val consumeRef = Consume.create(channels, patterns, continuation, true, 0)

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
        throw new RuntimeException(s"Install never result in an invalid match: $e")
      case Right(None) =>
        installs.update(_.updated(channels, Install(patterns, continuation, m)))
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
        throw new RuntimeException("Installing can be done only on startup")
    }

  }

  override def install(channels: Seq[C], patterns: Seq[P], continuation: K)(
      implicit m: Match[P, E, A, R]
  ): F[Option[(K, Seq[R])]] =
    store.withTxnF(store.createTxnWriteF()) { txn =>
      install(txn, channels, patterns, continuation)
    }

  override def retrieve(
      root: Blake2b256Hash,
      channelsHash: Blake2b256Hash
  ): F[Option[GNAT[C, P, A, K]]] =
    syncF.delay {
      history.lookup(store.trieStore, root, channelsHash)
    }

  override def reset(root: Blake2b256Hash): F[Unit] =
    store.withTxnF(store.createTxnWriteF()) { txn =>
      store.withTrieTxn(txn) { trieTxn =>
        store.trieStore.validateAndPutRoot(trieTxn, store.trieBranch, root)
        val leaves = store.trieStore.getLeaves(trieTxn, root)
        eventLog.update(kp(Seq.empty))
        store.getAndClearTrieUpdates()
        store.clear(txn)
        restoreInstalls(txn)
        store.bulkInsert(txn, leaves.map { case Leaf(k, v) => (k, v) })
      }
    }

  override def clear(): F[Unit] =
    store
      .withTxnF(store.createTxnReadF()) { txn =>
        store.withTrieTxn(txn) { trieTxn =>
          store.trieStore.getEmptyRoot(trieTxn)
        }
      }
      .flatMap(root => reset(root))

}
