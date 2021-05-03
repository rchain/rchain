package coop.rchain.rspace

import cats.Parallel
import cats.effect._
import cats.syntax.all._
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib._
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.implicits._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.rspace.history._
import coop.rchain.rspace.internal.{ConsumeCandidate, Datum, ProduceCandidate, WaitingContinuation}
import coop.rchain.rspace.trace._
import coop.rchain.rspace.syntax._
import coop.rchain.shared.SyncVarOps._
import coop.rchain.shared.{Log, Serialize}
import coop.rchain.store.KeyValueStore
import monix.execution.atomic.AtomicAny

import java.lang
import scala.collection.SortedSet
import scala.concurrent.ExecutionContext

class RSpace[F[_]: Concurrent: ContextShift: Log: Metrics: Span, C, P, A, K](
    historyRepository: HistoryRepository[F, C, P, A, K],
    storeAtom: AtomicAny[HotStore[F, C, P, A, K]]
)(
    implicit
    serializeC: Serialize[C],
    serializeP: Serialize[P],
    serializeA: Serialize[A],
    serializeK: Serialize[K],
    val m: Match[F, P, A],
    scheduler: ExecutionContext
) extends RSpaceOps[F, C, P, A, K](historyRepository, storeAtom)
    with ISpace[F, C, P, A, K] {

  def store: HotStore[F, C, P, A, K] = storeAtom.get()

  protected[this] override val logger: Logger = Logger[this.type]

  implicit protected[this] lazy val MetricsSource: Source = RSpaceMetricsSource

  protected[this] override def lockedConsume(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      peeks: SortedSet[Int],
      consumeRef: Consume
  ): F[MaybeActionResult] =
    Span[F].traceI("locked-consume") {
      for {
        _ <- Log[F].debug(
              s"consume: searching for data matching <patterns: $patterns> at <channels: $channels>"
            )
        _                    <- logConsume(consumeRef, channels, patterns, continuation, persist, peeks)
        channelToIndexedData <- fetchChannelToIndexData(channels)
        options <- extractDataCandidates(
                    channels.zip(patterns),
                    channelToIndexedData,
                    Nil
                  ).map(_.sequence)
        wk = WaitingContinuation(patterns, continuation, persist, peeks, consumeRef)
        result <- options.fold(storeWaitingContinuation(channels, wk))(
                   dataCandidates =>
                     for {
                       _ <- logComm(
                             dataCandidates,
                             channels,
                             wk,
                             COMM(
                               dataCandidates,
                               consumeRef,
                               peeks,
                               produceCounters _
                             ),
                             consumeCommLabel
                           )
                       _ <- storePersistentData(dataCandidates, peeks)
                       _ <- Log[F].debug(
                             s"consume: data found for <patterns: $patterns> at <channels: $channels>"
                           )
                     } yield wrapResult(channels, wk, consumeRef, dataCandidates)
                 )
      } yield result
    }

  /*
   * Here, we create a cache of the data at each channel as `channelToIndexedData`
   * which is used for finding matches.  When a speculative match is found, we can
   * remove the matching datum from the remaining data candidates in the cache.
   *
   * Put another way, this allows us to speculatively remove matching data without
   * affecting the actual store contents.
   */
  private[this] def fetchChannelToIndexData(channels: Seq[C]): F[Map[C, Seq[(Datum[A], Int)]]] =
    channels
      .traverse { c: C =>
        store.getData(c).shuffleWithIndex.map(c -> _)
      }
      .map(_.toMap)

  protected[this] override def lockedProduce(
      channel: C,
      data: A,
      persist: Boolean,
      produceRef: Produce
  ): F[MaybeActionResult] =
    Span[F].traceI("locked-produce") {
      for {
        //TODO fix double join fetch
        groupedChannels <- store.getJoins(channel)
        _ <- Log[F].debug(
              s"produce: searching for matching continuations at <groupedChannels: $groupedChannels>"
            )
        _ <- logProduce(produceRef, channel, data, persist)
        extracted <- extractProduceCandidate(
                      groupedChannels,
                      channel,
                      Datum(data, persist, produceRef)
                    )
        r <- extracted.fold(storeData(channel, data, persist, produceRef))(processMatchFound)
      } yield r
    }

  /*
   * Find produce candidate
   */
  private[this] def extractProduceCandidate(
      groupedChannels: Seq[CandidateChannels],
      batChannel: C,
      data: Datum[A]
  ): F[MaybeProduceCandidate] =
    runMatcherForChannels(
      groupedChannels,
      channels =>
        store
          .getContinuations(channels)
          .shuffleWithIndex,
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
      c =>
        store
          .getData(c)
          .shuffleWithIndex
          .map { d =>
            if (c == batChannel) (data, -1) +: d else d
          }
          .map(c -> _)
    )

  private[this] def processMatchFound(
      pc: ProduceCandidate[C, P, A, K]
  ): F[MaybeActionResult] = {
    val ProduceCandidate(
      channels,
      wk @ WaitingContinuation(_, _, persistK, peeks, consumeRef),
      continuationIndex,
      dataCandidates
    ) = pc

    for {
      _ <- logComm(
            dataCandidates,
            channels,
            wk,
            COMM(dataCandidates, consumeRef, peeks, produceCounters _),
            produceCommLabel
          )
      _ <- store.removeContinuation(channels, continuationIndex).unlessA(persistK)
      _ <- removeMatchedDatumAndJoin(channels, dataCandidates)
      _ <- Log[F].debug(s"produce: matching continuation found at <channels: $channels>")
    } yield wrapResult(channels, wk, consumeRef, dataCandidates)
  }

  protected override def logComm(
      dataCandidates: Seq[ConsumeCandidate[C, A]],
      channels: Seq[C],
      wk: WaitingContinuation[P, K],
      comm: COMM,
      label: String
  ): F[COMM] =
    Metrics[F].incrementCounter(label).map { _ =>
      eventLog.update(comm +: _)
      comm
    }

  protected override def logConsume(
      consumeRef: Consume,
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      peeks: SortedSet[Int]
  ): F[Consume] = syncF.delay {
    eventLog.update(consumeRef +: _)
    consumeRef
  }

  protected override def logProduce(
      produceRef: Produce,
      channel: C,
      data: A,
      persist: Boolean
  ): F[Produce] = syncF.delay {
    eventLog.update(produceRef +: _)
    if (!persist)
      produceCounter.update(_.putAndIncrementCounter(produceRef))
    produceRef
  }

  override def createCheckpoint(): F[Checkpoint] = spanF.withMarks("create-checkpoint") {
    for {
      changes <- spanF.withMarks("changes") { storeAtom.get().changes() }
      nextHistory <- spanF.withMarks("history-checkpoint") {
                      historyRepositoryAtom.get().checkpoint(changes.toList)
                    }
      _ = historyRepositoryAtom.set(nextHistory)
      _ <- createNewHotStore(nextHistory.getHistoryReader(nextHistory.root))(
            serializeK.toSizeHeadCodec
          )
      log = eventLog.take()
      _   = eventLog.put(Seq.empty)
      _   = produceCounter.take()
      _   = produceCounter.put(Map.empty.withDefaultValue(0))
      _   <- restoreInstalls()
    } yield Checkpoint(nextHistory.history.root, log)
  }

  def spawn: F[ISpace[F, C, P, A, K]] = spanF.withMarks("spawn") {
    val historyRep  = historyRepositoryAtom.get()
    implicit val ck = serializeK.toSizeHeadCodec
    for {
      nextHistory <- historyRep.reset(historyRep.history.root)
      hotStore    <- HotStore.empty(nextHistory.getHistoryReader(nextHistory.root).toRho)
      r           = new RSpace[F, C, P, A, K](nextHistory, AtomicAny(hotStore))
      _           <- r.restoreInstalls()
    } yield r
  }
}

object RSpace {
  val parallelism = lang.Runtime.getRuntime.availableProcessors() * 2

  final case class RSpaceStore[F[_]](
      history: KeyValueStore[F],
      roots: KeyValueStore[F],
      cold: KeyValueStore[F],
      channels: KeyValueStore[F]
  )

  def createPlay[F[_]: Concurrent: Parallel: ContextShift: Span: Metrics: Log, C, P, A, K](
      historyRepository: HistoryRepository[F, C, P, A, K],
      store: HotStore[F, C, P, A, K]
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      m: Match[F, P, A],
      scheduler: ExecutionContext
  ): F[ISpace[F, C, P, A, K]] =
    Sync[F].delay(new RSpace[F, C, P, A, K](historyRepository, AtomicAny(store)))

  def createReplay[F[_]: Concurrent: ContextShift: Log: Metrics: Span, C, P, A, K](
      historyRepository: HistoryRepository[F, C, P, A, K],
      store: HotStore[F, C, P, A, K]
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      m: Match[F, P, A],
      scheduler: ExecutionContext
  ): F[IReplaySpace[F, C, P, A, K]] = {
    val space: IReplaySpace[F, C, P, A, K] =
      new ReplayRSpace[F, C, P, A, K](historyRepository, AtomicAny(store))
    space.pure[F]
  }

  def createWithReplay[F[_]: Concurrent: Parallel: ContextShift: Span: Metrics: Log, C, P, A, K](
      store: RSpaceStore[F]
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      m: Match[F, P, A],
      scheduler: ExecutionContext
  ): F[(ISpace[F, C, P, A, K], IReplaySpace[F, C, P, A, K], HistoryRepository[F, C, P, A, K])] = {
    implicit val cc = sc.toSizeHeadCodec
    for {
      setup                  <- setUp[F, C, P, A, K](store)
      (historyReader, store) = setup
      space                  = new RSpace[F, C, P, A, K](historyReader, AtomicAny(store))
      replayStore <- HotStore.empty(historyReader.getHistoryReader(historyReader.root).toRho)(
                      sk.toSizeHeadCodec,
                      Concurrent[F]
                    )
      replay = new ReplayRSpace[F, C, P, A, K](
        historyReader,
        AtomicAny(replayStore)
      )
    } yield (space, replay, historyReader)
  }

  def create[F[_]: Concurrent: Parallel: ContextShift: Span: Metrics: Log, C, P, A, K](
      store: RSpaceStore[F]
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      m: Match[F, P, A],
      scheduler: ExecutionContext
  ): F[ISpace[F, C, P, A, K]] =
    for {
      setup                  <- setUp[F, C, P, A, K](store)
      (historyReader, store) = setup
      space = new RSpace[F, C, P, A, K](
        historyReader,
        AtomicAny(store)
      )
    } yield space

  def setUp[F[_]: Concurrent: Parallel: Log: Span, C, P, A, K](store: RSpaceStore[F])(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K]
  ): F[(HistoryRepository[F, C, P, A, K], HotStore[F, C, P, A, K])] = {
    import coop.rchain.rspace.history._

    for {
      historyRepo <- HistoryRepositoryInstances.lmdbRepository[F, C, P, A, K](
                      store.history,
                      store.roots,
                      store.cold,
                      store.channels
                    )
      store <- HotStore.empty(historyRepo.getHistoryReader(historyRepo.root).toRho)
    } yield (historyRepo, store)
  }
}
