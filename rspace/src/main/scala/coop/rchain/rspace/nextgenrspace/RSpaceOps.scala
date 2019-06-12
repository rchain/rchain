package coop.rchain.rspace.nextgenrspace

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib._
import coop.rchain.rspace._
import coop.rchain.rspace.concurrent.{ConcurrentTwoStepLockF, TwoStepLock}
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.Consume
import coop.rchain.rspace.nextgenrspace.history.{History, HistoryRepository}
import coop.rchain.shared.{Cell, Log}
import coop.rchain.shared.SyncVarOps._
import monix.execution.atomic.AtomicAny

import scala.concurrent.SyncVar
import scala.util.Random
import scodec.Codec

abstract class RSpaceOps[F[_]: Concurrent, C, P, A, R, K](
    historyRepository: HistoryRepository[F, C, P, A, K],
    val storeAtom: AtomicAny[HotStore[F, C, P, A, K]],
    val branch: Branch
)(
    implicit
    serializeC: Serialize[C],
    serializeP: Serialize[P],
    serializeK: Serialize[K],
    logF: Log[F]
) extends SpaceMatcher[F, C, P, A, R, K] {

  //TODO close in some F state abstraction
  protected val historyRepositoryAtom: AtomicAny[HistoryRepository[F, C, P, A, K]] = AtomicAny(
    historyRepository
  )

  def store: HotStore[F, C, P, A, K]

  def getData(channel: C): F[Seq[Datum[A]]] =
    store.getData(channel)

  def getWaitingContinuations(channels: Seq[C]): F[Seq[WaitingContinuation[P, K]]] =
    store.getContinuations(channels)

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
        for {
          groupedChannels <- store.getJoins(channel)
        } yield (groupedChannels.flatten.map(StableHashProvider.hash(_)))
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
        channelToIndexedData <- channels
                                 .traverse { c =>
                                   for {
                                     data <- store.getData(c)
                                   } yield c -> Random.shuffle(data.zipWithIndex)
                                 }
        options <- extractDataCandidates(channels.zip(patterns), channelToIndexedData.toMap, Nil)
                    .map(_.sequence)
        result <- options match {
                   case None =>
                     for {
                       _ <- syncF.delay {
                             installs.update(
                               _.updated(channels, Install(patterns, continuation, m))
                             )
                           }
                       _ <- store.installContinuation(
                             channels,
                             WaitingContinuation(
                               patterns,
                               continuation,
                               persist = true,
                               consumeRef
                             )
                           )
                       _ <- channels.traverse { channel =>
                             store.installJoin(channel, channels)
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
  ): F[Option[GNAT[C, P, A, K]]] = ???

  protected[rspace] def isDirty(root: Blake2b256Hash): F[Boolean]

  def toMap: F[Map[Seq[C], Row[P, A, K]]] = storeAtom.get().toMap

  override def reset(root: Blake2b256Hash): F[Unit] =
    for {
      nextHistory <- historyRepositoryAtom.get().reset(root)
      _           = historyRepositoryAtom.set(nextHistory)
      _           = eventLog.take()
      _           = eventLog.put(Seq.empty)
      _           <- createNewHotStore(nextHistory)(serializeK.toCodec)
      _           <- restoreInstalls()
    } yield ()

  override def clear(): F[Unit] = reset(History.emptyRootHash)

  protected def createCache: F[Cell[F, Cache[C, P, A, K]]] =
    Cell.refCell[F, Cache[C, P, A, K]](Cache())

  protected def createNewHotStore(
      historyReader: HistoryReader[F, C, P, A, K]
  )(implicit ck: Codec[K]): F[Unit] =
    for {
      cache        <- createCache
      nextHotStore = HotStore.inMem(Sync[F], cache, historyReader, ck)
      _            = storeAtom.set(nextHotStore)
    } yield ()

}
