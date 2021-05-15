package coop.rchain.rspace

import cats.Applicative
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.syntax.all._
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib._
import coop.rchain.metrics.implicits._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.rspace.concurrent.ConcurrentTwoStepLockF
import coop.rchain.rspace.hashing.{Blake2b256Hash, StableHashProvider}
import coop.rchain.rspace.history._
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.{COMM, Consume, Produce, Log => EventLog}
import coop.rchain.shared.SyncVarOps._
import coop.rchain.shared.{Log, Serialize}
import monix.execution.atomic.AtomicAny

import scala.collection.SortedSet
import scala.concurrent.{ExecutionContext, SyncVar}
import scala.util.Random

abstract class RSpaceOps[F[_]: Concurrent: ContextShift: Log: Metrics: Span, P, A, K](
    historyRepository: HistoryRepository[F, Channel, P, A, K],
    val storeAtom: AtomicAny[HotStore[F, Channel, P, A, K]]
)(
    implicit
    serializeP: Serialize[P],
    serializeA: Serialize[A],
    serializeK: Serialize[K],
    scheduler: ExecutionContext
) extends SpaceMatcher[F, Channel, P, A, K] {

  override def syncF: Sync[F] = Sync[F]
  override def spanF: Span[F] = Span[F]

  type MaybeProduceCandidate = Option[ProduceCandidate[P, A, K]]
  type MaybeActionResult     = Option[(ContResult[P, K], Seq[Result[A]])]
  type CandidateChannels     = Seq[Channel]

  implicit class MapOps(underlying: Map[Produce, Int]) {
    def putAndIncrementCounter(elem: Produce): Map[Produce, Int] =
      underlying
        .get(elem)
        .fold(underlying.+((elem, 1)))(currentCount => underlying.+((elem, currentCount + 1)))
  }

  implicit class RichFSeq[M[_]: Applicative, D](t: M[Seq[D]]) {
    def shuffleWithIndex: M[Seq[(D, Int)]] =
      t.map(d => Random.shuffle(d.zipWithIndex))
  }

  def assertF(predicate: Boolean, errorMsg: => String): F[Unit] =
    Sync[F].raiseError(new IllegalStateException(errorMsg)).unlessA(predicate)

  protected[this] val eventLog: SyncVar[EventLog] = create[EventLog](Seq.empty)
  protected[this] val produceCounter: SyncVar[Map[Produce, Int]] =
    create[Map[Produce, Int]](Map.empty.withDefaultValue(0))

  protected[this] def produceCounters(produceRefs: Seq[Produce]): Map[Produce, Int] =
    produceRefs
      .map(p => p -> produceCounter.get(p))
      .toMap

  private val lockF = new ConcurrentTwoStepLockF[F, Blake2b256Hash](MetricsSource)

  //private[this] val installSpanLabel         = Metrics.Source(MetricsSource, "install")
  //private[this] val restoreInstallsSpanLabel = Metrics.Source(MetricsSource, "restore-installs")
  //private[this] val createSoftCheckpointSpanLabel =
  //  Metrics.Source(MetricsSource, "create-soft-checkpoint")
  private[this] val revertSoftCheckpointSpanLabel =
    Metrics.Source(MetricsSource, "revert-soft-checkpoint")
  private[this] val resetSpanLabel = Metrics.Source(MetricsSource, "reset")

  protected[this] val consumeCommLabel     = "comm.consume"
  protected[this] val consumeTimeCommLabel = "comm.consume-time"
  protected[this] val produceCommLabel     = "comm.produce"
  protected[this] val produceTimeCommLabel = "comm.produce-time"

  //TODO close in some F state abstraction
  protected val historyRepositoryAtom: AtomicAny[HistoryRepository[F, Channel, P, A, K]] =
    AtomicAny(historyRepository)

  protected[this] val logger: Logger

  private[this] val installs: SyncVar[Installs[F, Channel, P, A, K]] = {
    val installs = new SyncVar[Installs[F, Channel, P, A, K]]()
    installs.put(Map.empty)
    installs
  }

  def store: HotStore[F, Channel, P, A, K]

  def getData(channel: Channel): F[Seq[Datum[A]]] =
    store.getData(channel)

  def getWaitingContinuations(channels: Seq[Channel]): F[Seq[WaitingContinuation[P, K]]] =
    store.getContinuations(channels)

  def getJoins(channel: Channel): F[Seq[Seq[Channel]]] =
    store.getJoins(channel)

  protected[this] def consumeLockF(
      channels: Seq[Channel]
  )(
      thunk: => F[MaybeActionResult]
  ): F[MaybeActionResult] = {
    val hashes = channels.map(_.hash)
    lockF.acquire(hashes)(() => hashes.pure[F])(thunk)
  }

  protected[this] def produceLockF(channel: Channel)(
      thunk: => F[MaybeActionResult]
  ): F[MaybeActionResult] =
    lockF.acquire(Seq(channel.hash))(
      () => store.getJoins(channel).map(_.flatten.map(_.hash))
    )(thunk)

  protected[this] def installLockF(channels: Seq[Channel])(
      thunk: => F[Option[(K, Seq[A])]]
  ): F[Option[(K, Seq[A])]] = {
    val hashes = channels.map(_.hash)
    lockF.acquire(hashes)(() => hashes.pure[F])(thunk)
  }

  protected[this] def storeWaitingContinuation(
      channels: Seq[Channel],
      wc: WaitingContinuation[P, K]
  ): F[MaybeActionResult] =
    for {
      _ <- store.putContinuation(channels, wc)
      _ <- channels.traverse(channel => store.putJoin(channel, channels))
      _ <- Log[F].debug(s"""|consume: no data found,
                          |storing <(patterns, continuation): (${wc.patterns}, ${wc.continuation})>
                          |at <channels: ${channels}>""".stripMargin.replace('\n', ' '))
    } yield None

  protected[this] def storeData(
      channel: Channel,
      data: A,
      persist: Boolean,
      produceRef: Produce
  ): F[MaybeActionResult] =
    for {
      _ <- Log[F].debug(s"produce: no matching continuation found")
      _ <- store.putDatum(channel, Datum(data, persist, produceRef))
      _ <- Log[F].debug(s"produce: persisted <data: $data> at <channel: $channel>")
    } yield None

  protected[this] def storePersistentData(
      dataCandidates: Seq[ConsumeCandidate[A]],
      peeks: SortedSet[Int]
  ): F[List[Unit]] =
    dataCandidates.toList
      .sortBy(_.datumIndex)(Ordering[Int].reverse)
      .traverse {
        case ConsumeCandidate(
            candidateChannel,
            Datum(_, persistData, _),
            _,
            dataIndex
            ) =>
          store.removeDatum(candidateChannel, dataIndex).unlessA(persistData)
      }

  def restoreInstalls(): F[Unit] =
    /*spanF.trace(restoreInstallsSpanLabel)*/
    installs.get.toList
      .traverse {
        case (channels, Install(patterns, continuation)) =>
          install(channels, patterns, continuation)
      }
      .as(())

  override def consume(
      channels: Seq[Channel],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      peeks: SortedSet[Int] = SortedSet.empty
  ): F[MaybeActionResult] =
    ContextShift[F].evalOn(scheduler) {
      if (channels.isEmpty) {
        val msg = "channels can't be empty"
        Log[F].error(msg) >> Sync[F]
          .raiseError[MaybeActionResult](new IllegalArgumentException(msg))
      } else if (channels.length =!= patterns.length) {
        val msg = "channels.length must equal patterns.length"
        Log[F].error(msg) >> Sync[F]
          .raiseError[MaybeActionResult](new IllegalArgumentException(msg))
      } else
        (for {
          consumeRef <- Sync[F].delay(Consume(channels, patterns, continuation, persist))
          result <- consumeLockF(channels) {
                     lockedConsume(
                       channels,
                       patterns,
                       continuation,
                       persist,
                       peeks,
                       consumeRef
                     )
                   }
        } yield result).timer(consumeTimeCommLabel)(Metrics[F], MetricsSource)
    }

  protected[this] def lockedConsume(
      channels: Seq[Channel],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      peeks: SortedSet[Int],
      consumeRef: Consume
  ): F[MaybeActionResult]

  override def produce(
      channel: Channel,
      data: A,
      persist: Boolean
  ): F[MaybeActionResult] =
    ContextShift[F].evalOn(scheduler) {
      (for {
        produceRef <- Sync[F].delay(Produce(channel, data, persist))
        result <- produceLockF(channel)(
                   lockedProduce(channel, data, persist, produceRef)
                 )
      } yield result).timer(produceTimeCommLabel)(Metrics[F], MetricsSource)
    }

  protected[this] def lockedProduce(
      channel: Channel,
      data: A,
      persist: Boolean,
      produceRef: Produce
  ): F[MaybeActionResult]

  override def install(
      channels: Seq[Channel],
      patterns: Seq[P],
      continuation: K
  ): F[Option[(K, Seq[A])]] =
    /* spanF.trace(installSpanLabel) */
    installLockF(channels) {
      implicit val ms = MetricsSource
      lockedInstall(channels, patterns, continuation).timer("install-time")
    }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  // TODO stop throwing exceptions
  private[this] def lockedInstall(
      channels: Seq[Channel],
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
        _ <- Log[F].debug(
              s"install: searching for data matching <patterns: $patterns> at <channels: $channels>"
            )
        consumeRef = Consume(channels, patterns, continuation, true)
        channelToIndexedData <- channels
                                 .traverse { c =>
                                   store.getData(c).shuffleWithIndex.map(c -> _)
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
                       _ <- Log[F].debug(
                             s"storing <(patterns, continuation): ($patterns, $continuation)> at <channels: $channels>"
                           )
                     } yield None
                   case Some(_) =>
                     throw new RuntimeException("Installing can be done only on startup")
                 }
      } yield result
    }

  def toMap: F[Map[Seq[Channel], Row[P, A, K]]] = storeAtom.get().toMap

  override def reset(root: Blake2b256Hash): F[Unit] = spanF.trace(resetSpanLabel) {
    for {
      nextHistory <- historyRepositoryAtom.get().reset(root)
      _           = historyRepositoryAtom.set(nextHistory)
      _           = eventLog.take()
      _           = eventLog.put(Seq.empty)
      _           = produceCounter.take()
      _           = produceCounter.put(Map.empty.withDefaultValue(0))
      _           <- createNewHotStore(nextHistory.getHistoryReader(root))
      _           <- restoreInstalls()

      // TODO: temp fix to release Semaphores inside TwoStepLock
      //  Adjust when runtime changes got in, create instance on spawn runtime.
      _ <- lockF.cleanUp
    } yield ()
  }

  override def clear(): F[Unit] = reset(History.emptyRootHash)

  protected def createNewHotStore(
      historyReader: HistoryReader[F, Blake2b256Hash, P, A, K]
  ): F[Unit] =
    for {
      nextHotStore <- HotStore.empty(historyReader.base)
      _            = storeAtom.set(nextHotStore)
    } yield ()

  override def createSoftCheckpoint(): F[SoftCheckpoint[Channel, P, A, K]] =
    /*spanF.trace(createSoftCheckpointSpanLabel) */
    for {
      cache    <- storeAtom.get().snapshot()
      log      = eventLog.take()
      _        = eventLog.put(Seq.empty)
      pCounter = produceCounter.take()
      _        = produceCounter.put(Map.empty.withDefaultValue(0))
    } yield SoftCheckpoint[Channel, P, A, K](cache, log, pCounter)

  override def revertToSoftCheckpoint(checkpoint: SoftCheckpoint[Channel, P, A, K]): F[Unit] =
    spanF.trace(revertSoftCheckpointSpanLabel) {
      val history = historyRepositoryAtom.get()
      for {
        hotStore <- HotStore.from(
                     checkpoint.cacheSnapshot.cache,
                     history.getHistoryReader(history.root).base
                   )
        _ = storeAtom.set(hotStore)
        _ = eventLog.take()

        _ = eventLog.put(checkpoint.log)
        _ = produceCounter.take()
        _ = produceCounter.put(checkpoint.produceCounter)
      } yield ()
    }

  def wrapResult(
      channels: Seq[Channel],
      wk: WaitingContinuation[P, K],
      consumeRef: Consume,
      dataCandidates: Seq[ConsumeCandidate[A]]
  ): MaybeActionResult =
    Some(
      (
        ContResult(
          wk.continuation,
          wk.persist,
          channels,
          wk.patterns,
          wk.peeks.nonEmpty
        ),
        dataCandidates
          .map(dc => Result(dc.channel, dc.datum.a, dc.removedDatum, dc.datum.persist))
      )
    )

  def removeMatchedDatumAndJoin(
      channels: Seq[Channel],
      dataCandidates: Seq[ConsumeCandidate[A]]
  ): F[Seq[Unit]] =
    dataCandidates
      .sortBy(_.datumIndex)(Ordering[Int].reverse)
      .traverse {
        case ConsumeCandidate(candidateChannel, Datum(_, persistData, _), _, dataIndex) => {
          store
            .removeDatum(candidateChannel, dataIndex)
            .whenA(dataIndex >= 0 && !persistData) >>
            store.removeJoin(candidateChannel, channels)
        }
      }

  protected[this] def runMatcherForChannels(
      groupedChannels: Seq[CandidateChannels],
      fetchMatchingContinuations: (CandidateChannels) => F[Seq[(WaitingContinuation[P, K], Int)]],
      fetchMatchingData: Channel => F[(Channel, Seq[(Datum[A], Int)])]
  ): F[MaybeProduceCandidate] = {
    def go(
        acc: Seq[CandidateChannels]
    ): F[Either[Seq[CandidateChannels], MaybeProduceCandidate]] =
      acc match {
        case Nil =>
          none[ProduceCandidate[P, A, K]].asRight[Seq[CandidateChannels]].pure[F]
        case channels :: remaining =>
          for {
            matchCandidates <- fetchMatchingContinuations(channels)
            channelToIndexedDataList <- channels.traverse { c: Channel =>
                                         fetchMatchingData(c)
                                       }
            firstMatch <- extractFirstMatch(
                           channels,
                           matchCandidates,
                           channelToIndexedDataList.toMap
                         )
          } yield firstMatch match {
            case None             => remaining.asLeft[MaybeProduceCandidate]
            case produceCandidate => produceCandidate.asRight[Seq[CandidateChannels]]
          }
      }
    groupedChannels.tailRecM(go)
  }

  protected def logComm(
      dataCandidates: Seq[ConsumeCandidate[A]],
      channels: Seq[Channel],
      wk: WaitingContinuation[P, K],
      comm: COMM,
      label: String
  ): F[COMM]

  protected def logConsume(
      consumeRef: Consume,
      channels: Seq[Channel],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      peeks: SortedSet[Int]
  ): F[Consume]

  protected def logProduce(
      produceRef: Produce,
      channel: Channel,
      data: A,
      persist: Boolean
  ): F[Produce]
}
