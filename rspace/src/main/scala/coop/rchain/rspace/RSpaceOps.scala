package coop.rchain.rspace

import scala.collection.SortedSet
import scala.concurrent.SyncVar
import scala.util.Random

import cats.effect.{Concurrent, Sync}
import cats.implicits._

import coop.rchain.catscontrib._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.metrics.implicits._
import coop.rchain.rspace.concurrent.{ConcurrentTwoStepLockF, TwoStepLock}
import coop.rchain.rspace.history._
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.{Consume, Log => EventLog}
import coop.rchain.shared.{Cell, Log}
import coop.rchain.shared.SyncVarOps._

import com.typesafe.scalalogging.Logger
import monix.execution.atomic.AtomicAny
import scodec.Codec

abstract class RSpaceOps[F[_]: Concurrent: Metrics, C, P, A, K](
    historyRepository: HistoryRepository[F, C, P, A, K],
    val storeAtom: AtomicAny[HotStore[F, C, P, A, K]],
    val branch: Branch
)(
    implicit
    serializeC: Serialize[C],
    serializeP: Serialize[P],
    serializeK: Serialize[K],
    logF: Log[F],
    spanF: Span[F]
) extends SpaceMatcher[F, C, P, A, K] {

  protected[this] val eventLog: SyncVar[EventLog] = create[EventLog](Seq.empty)

  private[this] val installSpanLabel         = Metrics.Source(MetricsSource, "install")
  private[this] val restoreInstallsSpanLabel = Metrics.Source(MetricsSource, "restore-installs")
  private[this] val createSoftCheckpointSpanLabel =
    Metrics.Source(MetricsSource, "create-soft-checkpoint")
  private[this] val revertSoftCheckpointSpanLabel =
    Metrics.Source(MetricsSource, "revert-soft-checkpoint")
  private[this] val resetSpanLabel = Metrics.Source(MetricsSource, "reset")

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

  private val lockF: TwoStepLock[F, Blake2b256Hash] = new ConcurrentTwoStepLockF(MetricsSource)

  type MaybeActionResult = Option[(ContResult[C, P, K], Seq[Result[C, A]])]

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
      thunk: => F[Option[(K, Seq[A])]]
  ): F[Option[(K, Seq[A])]] = {
    val hashes = channels.map(ch => StableHashProvider.hash(ch))
    lockF.acquire(hashes)(() => hashes.pure[F])(thunk)
  }

  protected[this] def storeWaitingContinuation(
      channels: Seq[C],
      wc: WaitingContinuation[P, K]
  ): F[MaybeActionResult] =
    for {
      _ <- store.putContinuation(channels, wc)
      _ <- channels.traverse(channel => store.putJoin(channel, channels))
      _ <- logF.debug(s"""|consume: no data found,
                          |storing <(patterns, continuation): (${wc.patterns}, ${wc.continuation})>
                          |at <channels: ${channels}>""".stripMargin.replace('\n', ' '))
    } yield None

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

  private[this] val installs: SyncVar[Installs[F, C, P, A, K]] = {
    val installs = new SyncVar[Installs[F, C, P, A, K]]()
    installs.put(Map.empty)
    installs
  }

  protected[this] def restoreInstalls(): F[Unit] = spanF.trace(restoreInstallsSpanLabel) {
    installs.get.toList
      .traverse {
        case (channels, Install(patterns, continuation)) =>
          install(channels, patterns, continuation)
      }
      .as(())
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  // TODO stop throwing exceptions
  private[this] def lockedInstall(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K
  ): F[Option[(K, Seq[A])]] =
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
                               _.updated(channels, Install(patterns, continuation))
                             )
                           }
                       _ <- store.installContinuation(
                             channels,
                             WaitingContinuation(
                               patterns,
                               continuation,
                               persist = true,
                               SortedSet.empty,
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

  override def install(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K
  ): F[Option[(K, Seq[A])]] = spanF.trace(installSpanLabel) {
    installLockF(channels) {
      implicit val ms = MetricsSource
      lockedInstall(channels, patterns, continuation).timer("install-time")
    }
  }

  def toMap: F[Map[Seq[C], Row[P, A, K]]] = storeAtom.get().toMap

  override def reset(root: Blake2b256Hash): F[Unit] = spanF.trace(resetSpanLabel) {
    for {
      nextHistory <- historyRepositoryAtom.get().reset(root)
      _           = historyRepositoryAtom.set(nextHistory)
      _           = eventLog.take()
      _           = eventLog.put(Seq.empty)
      _           <- createNewHotStore(nextHistory)(serializeK.toCodec)
      _           <- restoreInstalls()
    } yield ()
  }

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

  override def createSoftCheckpoint(): F[SoftCheckpoint[C, P, A, K]] =
    spanF.trace(createSoftCheckpointSpanLabel) {
      for {
        cache <- storeAtom.get().snapshot()
        log   = eventLog.take()
        _     = eventLog.put(Seq.empty)
      } yield SoftCheckpoint[C, P, A, K](cache, log)
    }

  override def revertToSoftCheckpoint(checkpoint: SoftCheckpoint[C, P, A, K]): F[Unit] =
    spanF.trace(revertSoftCheckpointSpanLabel) {
      implicit val ck: Codec[K] = serializeK.toCodec
      for {
        hotStore <- HotStore.from(checkpoint.cacheSnapshot.cache, historyRepository)
        _        = storeAtom.set(hotStore)
        _        = eventLog.take()
        _        = eventLog.put(checkpoint.log)
      } yield ()
    }

  override def close(): F[Unit] = historyRepository.close()

}
