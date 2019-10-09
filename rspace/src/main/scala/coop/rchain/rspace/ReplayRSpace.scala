package coop.rchain.rspace

import java.nio.file.Path

import scala.collection.JavaConverters._
import scala.collection.SortedSet
import scala.concurrent.ExecutionContext
import cats.Applicative
import cats.effect._
import cats.implicits._
import cats.temp.par.Par
import coop.rchain.catscontrib._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.metrics.implicits._
import coop.rchain.rspace.history.{Branch, HistoryRepository}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.{Produce, _}
import coop.rchain.shared.{Log, Serialize}
import coop.rchain.shared.SyncVarOps._
import com.google.common.collect.Multiset
import com.typesafe.scalalogging.Logger
import monix.execution.atomic.AtomicAny

class ReplayRSpace[F[_]: Sync, C, P, A, K](
    historyRepository: HistoryRepository[F, C, P, A, K],
    storeAtom: AtomicAny[HotStore[F, C, P, A, K]],
    branch: Branch
)(
    implicit
    serializeC: Serialize[C],
    serializeP: Serialize[P],
    serializeA: Serialize[A],
    serializeK: Serialize[K],
    val m: Match[F, P, A],
    val concurrent: Concurrent[F],
    protected val logF: Log[F],
    contextShift: ContextShift[F],
    scheduler: ExecutionContext,
    metricsF: Metrics[F],
    val spanF: Span[F]
) extends RSpaceOps[F, C, P, A, K](historyRepository, storeAtom, branch)
    with IReplaySpace[F, C, P, A, K] {

  protected[this] override val logger: Logger = Logger[this.type]

  implicit protected[this] lazy val MetricsSource: Metrics.Source =
    Metrics.Source(RSpaceMetricsSource, "replay")

  def store: HotStore[F, C, P, A, K] = storeAtom.get()

  private[this] val consumeCommLabel     = "comm.consume"
  private[this] val consumeTimeCommLabel = "comm.consume-time"
  private[this] val produceCommLabel     = "comm.produce"
  private[this] val produceTimeCommLabel = "comm.produce-time"

  def consume(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      peeks: SortedSet[Int] = SortedSet.empty
  ): F[MaybeActionResult] =
    contextShift.evalOn(scheduler) {
      if (channels.length =!= patterns.length) {
        val msg = "channels.length must equal patterns.length"
        logF.error(msg) >> syncF.raiseError(new IllegalArgumentException(msg))
      } else
        (for {
          result <- consumeLockF(channels) {
                     lockedConsume(
                       channels,
                       patterns,
                       continuation,
                       persist,
                       peeks
                     )
                   }
        } yield result).timer(consumeTimeCommLabel)
    }

  private[this] def lockedConsume(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      peeks: SortedSet[Int]
  ): F[MaybeActionResult] =
    for {
      _ <- logF.debug(
            s"consume: searching for data matching <patterns: $patterns> at <channels: $channels>"
          )
      consumeRef <- markConsume(channels, patterns, continuation, persist, peeks)
      r <- replayData
            .get(consumeRef)
            .fold(
              storeWaitingContinuation(
                channels,
                WaitingContinuation(patterns, continuation, persist, peeks, consumeRef)
              )
            )(
              comms =>
                getCommAndDataCandidates(channels, patterns, comms.iterator().asScala.toList)
                  .flatMap {
                    _.fold(
                      storeWaitingContinuation(
                        channels,
                        WaitingContinuation(patterns, continuation, persist, peeks, consumeRef)
                      )
                    ) {
                      case (comm, dataCandidates) =>
                        val channelsToIndex = channels.zipWithIndex.toMap
                        handleMatches(
                          channels,
                          patterns,
                          continuation,
                          persist,
                          dataCandidates,
                          consumeRef,
                          comms,
                          comm,
                          peeks,
                          channelsToIndex
                        )
                    }
                  }
            )
    } yield r

  def handleMatches(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      mats: Seq[DataCandidate[C, A]],
      consumeRef: Consume,
      comms: Multiset[COMM],
      comm: COMM,
      peeks: SortedSet[Int],
      channelsToIndex: Map[C, Int]
  ): F[MaybeActionResult] =
    for {
      commRef <- markComm(
                  consumeRef,
                  mats,
                  channels,
                  patterns,
                  continuation,
                  persist,
                  peeks,
                  comm,
                  consumeCommLabel
                )
      _ <- assertF(
            comms.contains(commRef),
            s"COMM Event $commRef was not contained in the trace $comms"
          )
      r <- for {
            _ <- mats.toList
                  .sortBy(_.datumIndex)(Ordering[Int].reverse)
                  .traverse {
                    case DataCandidate(candidateChannel, Datum(_, persistData, _), _, dataIndex) =>
                      store.removeDatum(candidateChannel, dataIndex).unlessA(persistData)
                  }
            _ <- logF.debug(
                  s"consume: data found for <patterns: $patterns> at <channels: $channels>"
                )
            _ <- removeBindingsFor(commRef)

          } yield wrapResult(channels, patterns, continuation, persist, peeks, consumeRef, mats)
    } yield r

// TODO: refactor this monster
  def getCommAndDataCandidates(
      channels: Seq[C],
      patterns: Seq[P],
      comms: Seq[COMM]
  ): F[Option[(COMM, Seq[DataCandidate[C, A]])]] = {
    type COMMOrData = Either[COMM, (COMM, Seq[DataCandidate[C, A]])]
    def go(
        comms: Seq[COMM]
    ): F[Either[Seq[COMM], COMMOrData]] =
      comms match {
        case Nil =>
          val msg = "List comms must not be empty"
          logger.error(msg)
          Sync[F].raiseError(new IllegalArgumentException(msg))
        case commRef :: Nil =>
          runMatcherC(channels, patterns, commRef).map {
            case Some(dataCandidates) =>
              (commRef, dataCandidates).asRight[COMM].asRight[Seq[COMM]]
            case None => commRef.asLeft[(COMM, Seq[DataCandidate[C, A]])].asRight[Seq[COMM]]
          }
        case commRef :: rem =>
          runMatcherC(channels, patterns, commRef).map {
            case Some(dataCandidates) =>
              (commRef, dataCandidates).asRight[COMM].asRight[Seq[COMM]]
            case None => rem.asLeft[COMMOrData]
          }
      }
    comms.tailRecM(go).map(_.toOption)
  }

  def runMatcherC(
      channels: Seq[C],
      patterns: Seq[P],
      comm: COMM
  ): F[Option[Seq[DataCandidate[C, A]]]] =
    for {
      channelToIndexedDataList <- channels.traverse { c: C =>
                                   store
                                     .getData(c)
                                     .map(_.zipWithIndex.filter(matches(comm)))
                                     .map(v => c -> v)
                                 }
      result <- extractDataCandidates(channels.zip(patterns), channelToIndexedDataList.toMap, Nil)
                 .map(_.sequence)
    } yield result

  def produce(channel: C, data: A, persist: Boolean): F[MaybeActionResult] =
    contextShift.evalOn(scheduler) {
      (for {
        result <- produceLockF(channel) {
                   lockedProduce(channel, data, persist)
                 }
      } yield result).timer(produceTimeCommLabel)
    }

  private[this] def lockedProduce(
      channel: C,
      data: A,
      persist: Boolean
  ): F[MaybeActionResult] =
    for {
      groupedChannels <- store.getJoins(channel)
      _ <- logF.debug(
            s"produce: searching for matching continuations at <groupedChannels: $groupedChannels>"
          )
      produceRef <- markProduce(channel, data, persist)
      result <- replayData.get(produceRef) match {
                 case None =>
                   storeDatum(channel, data, persist, produceRef, None)
                 case Some(comms) =>
                   val commOrProduceCandidate
                       : F[Either[COMM, (COMM, ProduceCandidate[C, P, A, K])]] =
                     getCommOrProduceCandidate(
                       channel,
                       data,
                       persist,
                       comms.iterator().asScala.toList,
                       produceRef,
                       groupedChannels
                     )
                   val r: F[MaybeActionResult] = commOrProduceCandidate.flatMap {
                     case Left(comm) =>
                       storeDatum(channel, data, persist, produceRef, Some(comm))
                     case Right((comm, produceCandidate)) =>
                       val indexedChannels = produceCandidate.channels.zipWithIndex.toMap
                       handleMatch(
                         produceCandidate,
                         produceRef,
                         persist,
                         comms,
                         comm,
                         indexedChannels
                       )
                   }
                   r
               }
    } yield result

  private[this] def storeDatum(
      channel: C,
      data: A,
      persist: Boolean,
      produceRef: Produce,
      maybeCommRef: Option[COMM]
  ): F[MaybeActionResult] =
    for {
      _ <- store.putDatum(channel, Datum(data, persist, produceRef))
      _ <- logF.debug(
            s"produce: no matching continuation found storing <data: $data> at <channel: $channel>"
          )
    } yield None

  type MaybeProduceCandidate = Option[ProduceCandidate[C, P, A, K]]

  private[this] def getCommOrProduceCandidate(
      channel: C,
      data: A,
      persist: Boolean,
      comms: Seq[COMM],
      produceRef: Produce,
      groupedChannels: Seq[Seq[C]]
  ): F[Either[COMM, (COMM, ProduceCandidate[C, P, A, K])]] = {
    type COMMOrProduce = Either[COMM, (COMM, ProduceCandidate[C, P, A, K])]
    def go(comms: Seq[COMM]): F[Either[Seq[COMM], COMMOrProduce]] =
      comms match {
        case Nil =>
          val msg = "comms must not be empty"
          logger.error(msg)
          Sync[F].raiseError(new IllegalArgumentException(msg))
        case commRef :: Nil =>
          runMatcherP(channel, data, persist, commRef, produceRef, groupedChannels) map {
            case Some(x) => (commRef, x).asRight[COMM].asRight[Seq[COMM]]
            case None    => commRef.asLeft[(COMM, ProduceCandidate[C, P, A, K])].asRight[Seq[COMM]]
          }
        case commRef :: rem =>
          runMatcherP(channel, data, persist, commRef, produceRef, groupedChannels) map {
            case Some(x) => (commRef, x).asRight[COMM].asRight[Seq[COMM]]
            case None    => rem.asLeft[COMMOrProduce]
          }
      }
    comms.tailRecM(go)
  }

  private[this] def runMatcherP(
      channel: C,
      data: A,
      persist: Boolean,
      comm: COMM,
      produceRef: Produce,
      groupedChannels: Seq[Seq[C]]
  ): F[MaybeProduceCandidate] = {

    def go(groupedChannels: Seq[Seq[C]]): F[Either[Seq[Seq[C]], MaybeProduceCandidate]] =
      groupedChannels match {
        case Nil => none[ProduceCandidate[C, P, A, K]].asRight[Seq[Seq[C]]].pure[F]
        case channels :: remaining =>
          for {
            continuations <- store.getContinuations(channels)
            matchCandidates = continuations.zipWithIndex.filter {
              case (WaitingContinuation(_, _, _, _, source), _) =>
                comm.consume == source
            }
            channelToIndexedDataList <- channels.traverse { c: C =>
                                         store
                                           .getData(c)
                                           .map(_.zipWithIndex)
                                           .map(
                                             as =>
                                               c -> {
                                                 if (c == channel)
                                                   Seq((Datum(data, persist, produceRef), -1))
                                                 else as
                                               }.filter { matches(comm) }
                                           )
                                       }
            firstMatch <- extractFirstMatch(
                           channels,
                           matchCandidates,
                           channelToIndexedDataList.toMap
                         )
          } yield firstMatch match {
            case None             => remaining.asLeft[MaybeProduceCandidate]
            case produceCandidate => produceCandidate.asRight[Seq[Seq[C]]]
          }
      }
    groupedChannels.tailRecM(go)
  }

  private[this] def handleMatch(
      mat: ProduceCandidate[C, P, A, K],
      produceRef: Produce,
      persist: Boolean,
      comms: Multiset[COMM],
      comm: COMM,
      channelsToIndex: Map[C, Int]
  ): F[MaybeActionResult] =
    mat match {
      case ProduceCandidate(
          channels,
          WaitingContinuation(patterns, continuation, persistK, peeks, consumeRef),
          continuationIndex,
          dataCandidates
          ) =>
        for {
          commRef <- markComm(
                      consumeRef,
                      dataCandidates,
                      channels,
                      patterns,
                      continuation,
                      persist,
                      peeks,
                      comm,
                      produceCommLabel
                    )
          _ <- assertF(
                comms.contains(commRef),
                s"COMM Event $commRef was not contained in the trace $comms"
              )
          _ <- store.removeContinuation(channels, continuationIndex).unlessA(persistK)
          _ <- dataCandidates.toList
                .sortBy(_.datumIndex)(Ordering[Int].reverse)
                .traverse {
                  case DataCandidate(candidateChannel, Datum(_, persistData, _), _, dataIndex) =>
                    store
                      .removeDatum(candidateChannel, dataIndex)
                      .whenA((dataIndex >= 0 && !persistData)) >>
                      store.removeJoin(candidateChannel, channels)
                }
          _ <- logF.debug(s"produce: matching continuation found at <channels: $channels>")
          _ <- removeBindingsFor(commRef)
          r <- syncF.delay {
                Some(
                  (
                    ContResult(
                      continuation,
                      persistK,
                      channels,
                      patterns,
                      peeks.nonEmpty
                    ),
                    dataCandidates.map(
                      dc => Result(dc.channel, dc.datum.a, dc.removedDatum, dc.datum.persist)
                    )
                  )
                )
              }
        } yield r
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
      changes     <- storeAtom.get().changes()
      nextHistory <- historyRepositoryAtom.get().checkpoint(changes.toList)
      _           = historyRepositoryAtom.set(nextHistory)
      _           <- createNewHotStore(nextHistory)(serializeK.toCodec)
      _           <- restoreInstalls()
    } yield (Checkpoint(nextHistory.history.root, Seq.empty))
  }

  override def clear(): F[Unit] = syncF.delay { replayData.clear() } >> super.clear()

  private[this] def matches(comm: COMM)(datumWithIndex: (Datum[A], _)) = {
    val (datum, _) = datumWithIndex
    def wasRepeatedEnoughTimes =
      (if (!datum.persist) {
         comm.timesRepeated(datum.source) === produceCounter.get(
           datum.source
         )
       } else true)
    comm.produces.contains(datum.source) && wasRepeatedEnoughTimes
  }

  protected def markComm(
      consumeRef: Consume,
      dataCandidates: Seq[DataCandidate[C, A]],
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      peeks: SortedSet[Int],
      comm: COMM,
      label: String
  ): F[COMM] =
    for {
      _ <- metricsF.incrementCounter(label)
      commRef <- syncF.delay {
                  COMM(consumeRef, dataCandidates.map(_.datum.source), peeks, comm.timesRepeated)
                }
    } yield commRef

  protected def markConsume(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      peeks: SortedSet[Int]
  ): F[Consume] =
    syncF.delay {
      Consume.create(channels, patterns, continuation, persist)
    }

  protected def markProduce(
      channel: C,
      data: A,
      persist: Boolean
  ): F[Produce] =
    syncF.delay {
      val ref = Produce.create(channel, data, persist)
      if (!persist) produceCounter.update(_.putAndIncrementCounter(ref))
      ref
    }
}

object ReplayRSpace {

  def create[F[_], C, P, A, K](
      historyRepository: HistoryRepository[F, C, P, A, K],
      store: HotStore[F, C, P, A, K],
      branch: Branch
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      m: Match[F, P, A],
      concurrent: Concurrent[F],
      logF: Log[F],
      contextShift: ContextShift[F],
      scheduler: ExecutionContext,
      metricsF: Metrics[F],
      spanF: Span[F]
  ): F[ReplayRSpace[F, C, P, A, K]] = {

    val space: ReplayRSpace[F, C, P, A, K] =
      new ReplayRSpace[F, C, P, A, K](historyRepository, AtomicAny(store), branch)

    space.pure[F]
  }

  def create[F[_], C, P, A, K](
      dataDir: Path,
      mapSize: Long,
      branch: Branch
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      m: Match[F, P, A],
      concurrent: Concurrent[F],
      logF: Log[F],
      contextShift: ContextShift[F],
      scheduler: ExecutionContext,
      metricsF: Metrics[F],
      spanF: Span[F],
      par: Par[F]
  ): F[IReplaySpace[F, C, P, A, K]] =
    RSpace.setUp[F, C, P, A, K](dataDir, mapSize, branch).map {
      case (historyReader, store) =>
        new ReplayRSpace[F, C, P, A, K](historyReader, AtomicAny(store), branch)
    }
}
