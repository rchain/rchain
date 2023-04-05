package coop.rchain.rspace

import cats.effect._
import cats.syntax.all._
import com.google.common.collect.Multiset
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib._
import coop.rchain.metrics.implicits._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.rspace.history.HistoryRepository
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace._
import coop.rchain.shared.syntax._
import coop.rchain.shared.{Log, Serialize}
import monix.execution.atomic.AtomicAny

import scala.collection.JavaConverters._
import scala.collection.{immutable, SortedSet}
import scala.concurrent.ExecutionContext

class ReplayRSpace[F[_]: Concurrent: ContextShift: Log: Metrics: Span, C, P, A, K](
    historyRepository: HistoryRepository[F, C, P, A, K],
    storeAtom: AtomicAny[HotStore[F, C, P, A, K]],
    rholangEC: ExecutionContext
)(
    implicit
    serializeC: Serialize[C],
    serializeP: Serialize[P],
    serializeA: Serialize[A],
    serializeK: Serialize[K],
    val m: Match[F, P, A]
) extends RSpaceOps[F, C, P, A, K](historyRepository, storeAtom, rholangEC)
    with IReplaySpace[F, C, P, A, K] {

  protected override def logF: Log[F] = Log[F]

  protected[this] override val logger: Logger = Logger[this.type]

  implicit protected[this] lazy val MetricsSource: Metrics.Source =
    Metrics.Source(RSpaceMetricsSource, "replay")

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
        _ <- logF.debug(
              s"consume: searching for data matching <patterns: $patterns> at <channels: $channels>"
            )
        _  <- logConsume(consumeRef, channels, patterns, continuation, persist, peeks)
        wk = WaitingContinuation(patterns, continuation, persist, peeks, consumeRef)
        r <- replayData
              .get(consumeRef)
              .fold(storeWaitingContinuation(channels, wk))(
                comms =>
                  getCommAndConsumeCandidates(channels, patterns, comms.iterator().asScala.toList)
                    .flatMap {
                      _.fold(storeWaitingContinuation(channels, wk)) {
                        case (_, dataCandidates) =>
                          for {
                            comm    <- COMM(dataCandidates, consumeRef, peeks, produceCounters _)
                            commRef <- logComm(dataCandidates, channels, wk, comm, consumeCommLabel)
                            _ <- assertF(
                                  comms.contains(commRef),
                                  s"COMM Event $commRef was not contained in the trace $comms"
                                )
                            _ <- storePersistentData(dataCandidates, peeks)
                            _ <- logF.debug(
                                  s"consume: data found for <patterns: $patterns> at <channels: $channels>"
                                )
                            _ <- removeBindingsFor(commRef)
                          } yield wrapResult(channels, wk, consumeRef, dataCandidates)
                      }
                    }
              )
      } yield r
    }

  def getCommAndConsumeCandidates(
      channels: Seq[C],
      patterns: Seq[P],
      comms: Seq[COMM]
  ): F[Option[(COMM, Seq[ConsumeCandidate[C, A]])]] =
    getCommOrCandidate(comms, comm => runMatcherConsume(channels, patterns, comm))

  def runMatcherConsume(
      channels: Seq[C],
      patterns: Seq[P],
      comm: COMM
  ): F[Option[Seq[ConsumeCandidate[C, A]]]] =
    for {
      channelToIndexedDataList <- channels.traverse { c: C =>
                                   store
                                     .getData(c)
                                     .flatMap(
                                       _.iterator.zipWithIndex
                                       // Convert to immutable.Seq because of defined Traversable instance
                                         .to[immutable.Seq]
                                         .filterA(matches(comm))
                                     )
                                     .map(c -> _)
                                 }
      result <- extractDataCandidates(channels.zip(patterns), channelToIndexedDataList.toMap, Nil)
                 .map(_.sequence)
    } yield result

  protected[this] override def lockedProduce(
      channel: C,
      data: A,
      persist: Boolean,
      produceRef: Produce
  ): F[MaybeActionResult] =
    Span[F].traceI("locked-produce") {
      for {
        groupedChannels <- store.getJoins(channel)
        _ <- logF.debug(
              s"produce: searching for matching continuations at <groupedChannels: $groupedChannels>"
            )
        _ <- logProduce(produceRef, channel, data, persist)
        result <- replayData.get(produceRef) match {
                   case None =>
                     storeData(channel, data, persist, produceRef)
                   case Some(comms) =>
                     getCommOrProduceCandidate(
                       channel,
                       data,
                       persist,
                       comms.iterator().asScala.toList,
                       produceRef,
                       groupedChannels
                     ).flatMap(
                       _.fold(storeData(channel, data, persist, produceRef)) {
                         case (_, pc) => handleMatch(pc, comms)
                       }
                     )
                 }
      } yield result
    }

  private[this] def getCommOrProduceCandidate(
      channel: C,
      data: A,
      persist: Boolean,
      comms: Seq[COMM],
      produceRef: Produce,
      groupedChannels: Seq[Seq[C]]
  ): F[Option[(COMM, ProduceCandidate[C, P, A, K])]] =
    getCommOrCandidate(
      comms,
      runMatcherProduce(channel, data, persist, _, produceRef, groupedChannels)
    )

  private[this] def runMatcherProduce(
      channel: C,
      data: A,
      persist: Boolean,
      comm: COMM,
      produceRef: Produce,
      groupedChannels: Seq[Seq[C]]
  ): F[MaybeProduceCandidate] =
    runMatcherForChannels(
      groupedChannels,
      channels =>
        store
          .getContinuations(channels)
          .map(_.iterator.zipWithIndex.filter {
            case (WaitingContinuation(_, _, _, _, source), _) =>
              comm.consume == source
          }.toSeq),
      c =>
        store
          .getData(c)
          // Convert to immutable.Seq because of defined Traversable instance
          .map(_.iterator.zipWithIndex.to[immutable.Seq])
          .flatMap { datums =>
            (if (c == channel) (Datum(data, persist, produceRef), -1) +: datums else datums)
              .filterA(matches(comm))
              .map(_.to[Seq])
          }
          .map(c -> _)
    )

  private[this] def matches(comm: COMM)(datumWithIndex: (Datum[A], _)): F[Boolean] = {
    val datum: Datum[A] = datumWithIndex._1
    def wasRepeatedEnoughTimes: F[Boolean] =
      produceCounter.get.map { counter =>
        if (!datum.persist) {
          comm.timesRepeated(datum.source) === counter(datum.source)
        } else true
      }
    Sync[F].delay(comm.produces.contains(datum.source)) &&^ wasRepeatedEnoughTimes
  }

  private[this] def handleMatch(
      pc: ProduceCandidate[C, P, A, K],
      comms: Multiset[COMM]
  ): F[MaybeActionResult] = {
    val ProduceCandidate(
      channels,
      wk @ WaitingContinuation(_, _, persistK, peeks, consumeRef),
      continuationIndex,
      dataCandidates
    ) = pc
    for {
      comm    <- COMM(dataCandidates, consumeRef, peeks, produceCounters _)
      commRef <- logComm(dataCandidates, channels, wk, comm, produceCommLabel)
      _ <- assertF(
            comms.contains(commRef),
            s"COMM Event $commRef was not contained in the trace $comms"
          )
      _ <- store.removeContinuation(channels, continuationIndex).unlessA(persistK)
      _ <- removeMatchedDatumAndJoin(channels, dataCandidates)
      _ <- logF.debug(s"produce: matching continuation found at <channels: $channels>")
      _ <- removeBindingsFor(commRef)
    } yield wrapResult(channels, wk, consumeRef, dataCandidates)
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  private def removeBindingsFor(
      commRef: COMM
  ): F[Unit] = Sync[F].delay {
    commRef.produces.foldLeft(replayData.removeBinding(commRef.consume, commRef)) {
      case (updatedReplays, produceRef) =>
        updatedReplays.removeBinding(produceRef, commRef)
    }
  }

  override def createCheckpoint(): F[Checkpoint] = checkReplayData >> syncF.defer {
    for {
      changes       <- storeAtom.get().changes
      nextHistory   <- historyRepositoryAtom.get().checkpoint(changes.toList)
      _             = historyRepositoryAtom.set(nextHistory)
      historyReader <- nextHistory.getHistoryReader(nextHistory.root)
      _             <- createNewHotStore(historyReader)
      _             <- restoreInstalls()
    } yield Checkpoint(nextHistory.history.root, Seq.empty)
  }

  override def clear(): F[Unit] = syncF.delay { replayData.clear() } >> super.clear()

  protected override def logComm(
      dataCandidates: Seq[ConsumeCandidate[C, A]],
      channels: Seq[C],
      wk: WaitingContinuation[P, K],
      comm: COMM,
      label: String
  ): F[COMM] =
    Metrics[F].incrementCounter(label).map(_ => comm)

  protected override def logConsume(
      consumeRef: Consume,
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      peeks: SortedSet[Int]
  ): F[Consume] = syncF.delay { consumeRef }

  protected override def logProduce(
      produceRef: Produce,
      channel: C,
      data: A,
      persist: Boolean
  ): F[Produce] =
    for {
      _ <- produceCounter.update(_.putAndIncrementCounter(produceRef)).whenA(!persist)
    } yield produceRef

  private[this] def getCommOrCandidate[Candidate](
      comms: Seq[COMM],
      runMatcher: COMM => F[Option[Candidate]]
  ): F[Option[(COMM, Candidate)]] = {
    type COMMOrCandidate = Either[COMM, (COMM, Candidate)]
    def go(
        cs: Seq[COMM]
    ): F[Either[Seq[COMM], COMMOrCandidate]] =
      cs match {
        case Nil =>
          val msg = "List comms must not be empty"
          logger.error(msg)
          Sync[F].raiseError(new IllegalArgumentException(msg))
        case commRef :: Nil =>
          runMatcher(commRef).map {
            case Some(dataCandidates) =>
              (commRef, dataCandidates).asRight[COMM].asRight[Seq[COMM]]
            case None => commRef.asLeft[(COMM, Candidate)].asRight[Seq[COMM]]
          }
        case commRef :: rem =>
          runMatcher(commRef).map {
            case Some(dataCandidates) =>
              (commRef, dataCandidates).asRight[COMM].asRight[Seq[COMM]]
            case None => rem.asLeft[COMMOrCandidate]
          }
      }
    comms.tailRecM(go).map(_.toOption)
  }

  def spawn: F[IReplaySpace[F, C, P, A, K]] = spanF.withMarks("spawn") {
    for {
      historyRepo   <- Sync[F].delay(historyRepositoryAtom.get())
      nextHistory   <- historyRepo.reset(historyRepo.history.root)
      historyReader <- nextHistory.getHistoryReader(nextHistory.root)
      hotStore      <- HotStore(historyReader.base)
      rSpaceReplay  <- ReplayRSpace(nextHistory, hotStore, rholangEC)
      _             <- rSpaceReplay.restoreInstalls()
    } yield rSpaceReplay
  }
}

object ReplayRSpace {

  /**
    * Creates [[ReplayRSpace]] from [[HistoryRepository]] and [[HotStore]].
    */
  def apply[F[_]: Concurrent: ContextShift: Log: Metrics: Span, C, P, A, K](
      historyRepository: HistoryRepository[F, C, P, A, K],
      store: HotStore[F, C, P, A, K],
      rholangEC: ExecutionContext
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      m: Match[F, P, A]
  ): F[ReplayRSpace[F, C, P, A, K]] = Sync[F].delay {
    new ReplayRSpace[F, C, P, A, K](historyRepository, AtomicAny(store), rholangEC)
  }

}
