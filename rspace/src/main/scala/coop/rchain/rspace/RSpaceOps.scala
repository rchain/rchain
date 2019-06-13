package coop.rchain.rspace

import cats.Monad
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.ski._
import coop.rchain.rspace.concurrent.{ConcurrentTwoStepLockF, TwoStepLock}
import coop.rchain.rspace.history.{Branch, Leaf}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.Consume
import coop.rchain.shared.Log
import coop.rchain.shared.SyncVarOps._

import scala.concurrent.SyncVar
import scala.util.Random

abstract class RSpaceOps[F[_]: Concurrent, C, P, A, R, K](
    val store: IStore[F, C, P, A, K],
    val branch: Branch
)(
    implicit
    serializeC: Serialize[C],
    serializeP: Serialize[P],
    serializeK: Serialize[K],
    logF: Log[F]
) extends SpaceMatcher[F, C, P, A, R, K] {

  implicit val codecC = serializeC.toCodec

  val syncF: Sync[F] = Concurrent[F]

  private val lockF: TwoStepLock[F, Blake2b256Hash] = new ConcurrentTwoStepLockF()

  type MaybeActionResult = Option[(ContResult[C, P, K], Seq[Result[R]])]

  protected[this] def consumeLockF(
      channels: Seq[C]
  )(
      thunk: => F[MaybeActionResult]
  ): F[MaybeActionResult] = {
    val hashes = channels.map(ch => StableHashProvider.hash(ch))
    lockF.acquire(hashes)(() => hashes.pure[F])(thunk)
  }

  protected[this] def installLockF(
      channels: Seq[C]
  )(
      thunk: => F[Option[(K, Seq[R])]]
  ): F[Option[(K, Seq[R])]] = {
    val hashes = channels.map(ch => StableHashProvider.hash(ch))
    lockF.acquire(hashes)(() => hashes.pure[F])(thunk)
  }

  protected[this] def produceLockF(
      channel: C
  )(
      thunk: => F[MaybeActionResult]
  ): F[MaybeActionResult] =
    lockF.acquire(Seq(StableHashProvider.hash(channel)))(
      () =>
        store.withReadTxnF { txn =>
          val groupedChannels: Seq[Seq[C]] = store.getJoin(txn, channel)
          groupedChannels.flatten.map(StableHashProvider.hash(_))
        }
    )(thunk)

  protected[this] val logger: Logger

  private[this] val installs: SyncVar[Installs[F, C, P, A, R, K]] = {
    val installs = new SyncVar[Installs[F, C, P, A, R, K]]()
    installs.put(Map.empty)
    installs
  }

  protected[this] def restoreInstalls(): F[Unit] =
    installs.get.toList
      .traverse {
        case (channels, Install(patterns, continuation, _match)) =>
          install(channels, patterns, continuation)(_match)
      }
      .as(())

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  // TODO stop throwing exceptions
  private[this] def lockedInstall(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K
  )(implicit m: Match[F, P, A, R]): F[Option[(K, Seq[R])]] =
    if (channels.length =!= patterns.length) {
      val msg = "channels.length must equal patterns.length"
      logger.error(msg)
      throw new IllegalArgumentException(msg)
    } else {
      /*
       * Here, we create a cache of the data at each channel as `channelToIndexedData`
       * which is used for finding matches.  When a speculative match is found, we can
       * remove the matching datum from the remaining data candidates in the cache.
       *
       * Put another way, this allows us to speculatively remove matching data without
       * affecting the actual store contents.
       */

      for {
        _          <- logF.debug(s"""|install: searching for data matching <patterns: $patterns>
                                  |at <channels: $channels>""".stripMargin.replace('\n', ' '))
        consumeRef = Consume.create(channels, patterns, continuation, true, 0)
        channelToIndexedData <- channels.toList
                                 .traverse { c: C =>
                                   store.withReadTxnF(
                                     txn =>
                                       c -> Random.shuffle(store.getData(txn, Seq(c)).zipWithIndex)
                                   )
                                 }
                                 .map(_.toMap)
        options <- extractDataCandidates(channels.zip(patterns), channelToIndexedData, Nil)
                    .map(_.sequence)
        result <- options match {
                   case None =>
                     for {
                       _ <- syncF.delay {
                             installs.update(
                               _.updated(channels, Install(patterns, continuation, m))
                             )
                           }
                       _ <- store.withWriteTxnF { txn =>
                             store.installWaitingContinuation(
                               txn,
                               channels,
                               WaitingContinuation(
                                 patterns,
                                 continuation,
                                 persist = true,
                                 Seq.empty,
                                 consumeRef
                               )
                             )
                             for (channel <- channels) store.addJoin(txn, channel, channels)
                           }

                       _ <- logF.debug(
                             s"""|storing <(patterns, continuation): ($patterns, $continuation)>
                              |at <channels: $channels>""".stripMargin.replace('\n', ' ')
                           )
                     } yield None
                   case Some(_) =>
                     throw new RuntimeException("Installing can be done only on startup")
                 }
      } yield result
    }

  override def install(channels: Seq[C], patterns: Seq[P], continuation: K)(
      implicit m: Match[F, P, A, R]
  ): F[Option[(K, Seq[R])]] =
    installLockF(channels) {
      lockedInstall(channels, patterns, continuation)
    }

  override def retrieve(
      root: Blake2b256Hash,
      channelsHash: Blake2b256Hash
  ): F[Option[GNAT[C, P, A, K]]] =
    syncF.delay {
      history.lookup(store.trieStore, root, channelsHash)
    }

  protected[rspace] def isDirty(root: Blake2b256Hash): F[Boolean]

  override def reset(root: Blake2b256Hash): F[Unit] =
    Monad[F].ifM(isDirty(root))(
      ifTrue = for {
        leaves <- store.withWriteTxnF { txn =>
                   store
                     .withTrieTxn(txn) { trieTxn =>
                       store.trieStore.validateAndPutRoot(trieTxn, store.trieBranch, root)
                       store.trieStore.getLeaves(trieTxn, root)
                     }
                 }
        _ <- syncF.delay { eventLog.update(kp(Seq.empty)) }
        _ <- syncF.delay { store.getAndClearTrieUpdates() }
        _ <- store.withWriteTxnF { txn =>
              store.clear(txn)
            }
        _ <- restoreInstalls()
        _ <- store.withWriteTxnF { txn =>
              store.bulkInsert(txn, leaves.map { case Leaf(k, v) => (k, v) })
            }
      } yield (),
      ifFalse = ().pure[F]
    )

  override def clear(): F[Unit] =
    store
      .withReadTxnF { txn =>
        store.withTrieTxn(txn) { trieTxn =>
          store.trieStore.getEmptyRoot(trieTxn)
        }
      }
      .flatMap(root => reset(root))

  override def toMap: F[Map[Seq[C], Row[P, A, K]]] = Sync[F].delay(store.toMap)

  override def createSoftCheckpoint(): F[SoftCheckpoint[C, P, A, K]] =
    createCheckpoint().map(c => SoftCheckpoint(Snapshot(Cache()), c.log, Some(c.root)))

  override def revertToSoftCheckpoint(checkpoint: SoftCheckpoint[C, P, A, K]): F[Unit] =
    reset(checkpoint.maybeRoot.get)
}
