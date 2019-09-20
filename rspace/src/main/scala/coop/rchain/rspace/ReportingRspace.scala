package coop.rchain.rspace

import scala.collection.JavaConverters._
import scala.collection.SortedSet
import cats.Applicative
import cats.effect._
import cats.implicits._
import coop.rchain.catscontrib._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.rspace.history.{Branch, HistoryRepository}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.{Produce, _}
import coop.rchain.shared.Log
import com.google.common.collect.Multiset
import com.typesafe.scalalogging.Logger
import coop.rchain.rspace.ReportingRspace.{
  ReportingComm,
  ReportingConsume,
  ReportingEvent,
  ReportingProduce
}
import monix.execution.atomic.AtomicAny
import coop.rchain.shared.SyncVarOps._

import scala.concurrent.SyncVar

object ReportingRspace {
  trait ReportingEvent

  final case class ReportingProduce[C, A](channel: C, data: A) extends ReportingEvent
  final case class ReportingConsume[C, P, K](
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      peeks: Seq[Int]
  ) extends ReportingEvent
  final case class ReportingComm[C, P, A, K](
      consume: ReportingConsume[C, P, K],
      produces: Seq[ReportingProduce[C, A]]
  ) extends ReportingEvent
}

class ReportingRspace[F[_]: Sync, C, P, A, K](
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
    metricsF: Metrics[F],
    val spanF: Span[F]
) extends RSpaceOps[F, C, P, A, K](historyRepository, storeAtom, branch)
    with IReplaySpace[F, C, P, A, K] {
  override def store: HotStore[F, C, P, A, K] = storeAtom.get()

  private[this] val consumeCommLabel = "comm.consume"
  private[this] val produceCommLabel = "comm.produce"

  protected[this] override val logger: Logger = Logger[this.type]

  implicit protected[this] lazy val MetricsSource: Metrics.Source =
    Metrics.Source(RSpaceMetricsSource, "reporting")

  val report: SyncVar[Seq[ReportingEvent]] = create[Seq[ReportingEvent]](Seq.empty)

  def getReport: F[Seq[ReportingEvent]] = Sync[F].delay(report.get)

  /** Creates a checkpoint.
    *
    * @return A [[Checkpoint]]
    */
  override def createCheckpoint(): F[Checkpoint] = checkReplayData >> syncF.defer {
    val historyRepository = historyRepositoryAtom.get()
    for {
      _ <- createNewHotStore(historyRepository)(serializeK.toCodec)
      _ <- restoreInstalls()
    } yield (Checkpoint(historyRepository.history.root, Seq.empty))
  }

  override def createSoftCheckpoint(): F[SoftCheckpoint[C, P, A, K]] =
    Sync[F].delay(report.update(_ => Seq.empty[ReportingEvent])) >> super.createSoftCheckpoint()

  def consume(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      sequenceNumber: Int,
      peeks: SortedSet[Int] = SortedSet.empty
  ): F[MaybeActionResult] =
    if (channels.length =!= patterns.length) {
      val msg = "channels.length must equal patterns.length"
      logF.error(msg) >> syncF.raiseError(new IllegalArgumentException(msg))
    } else
      for {
        result <- consumeLockF(channels) {
                   lockedConsume(
                     channels,
                     patterns,
                     continuation,
                     persist,
                     sequenceNumber,
                     peeks
                   )
                 }
      } yield result

  private[this] def lockedConsume(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      sequenceNumber: Int,
      peeks: SortedSet[Int]
  ): F[MaybeActionResult] = {
    def runMatcher(comm: COMM): F[Option[Seq[DataCandidate[C, A]]]] =
      for {
        channelToIndexedDataList <- channels.traverse { c: C =>
                                     store
                                       .getData(c)
                                       .map(_.zipWithIndex.filter {
                                         case (Datum(_, _, source), _) =>
                                           comm.produces.contains(source)
                                       })
                                       .map(v => c -> v)
                                   }
        result <- extractDataCandidates(channels.zip(patterns), channelToIndexedDataList.toMap, Nil)
                   .map(_.sequence)
      } yield result

    def handleMatches(
        mats: Seq[DataCandidate[C, A]],
        consumeRef: Consume,
        comms: Multiset[COMM],
        peeks: SortedSet[Int],
        channelsToIndex: Map[C, Int]
    ): F[MaybeActionResult] =
      for {
        _       <- metricsF.incrementCounter(consumeCommLabel)
        commRef <- syncF.delay { COMM(consumeRef, mats.map(_.datum.source), peeks) }
        _       <- assertF(comms.contains(commRef), "COMM Event was not contained in the trace")
        r <- mats.toList
              .sortBy(_.datumIndex)(Ordering[Int].reverse)
              .traverse {
                case DataCandidate(candidateChannel, Datum(_, persistData, _), _, dataIndex) =>
                  if (!persistData) {
                    store.removeDatum(candidateChannel, dataIndex)
                  } else ().pure[F]
              }
              .flatMap { _ =>
                logF.debug(
                  s"consume: data found for <patterns: $patterns> at <channels: $channels>"
                )
              }
              .flatMap { _ =>
                removeBindingsFor(commRef)
              }
              .map { _ =>
                val contSequenceNumber = commRef.nextSequenceNumber
                Some(
                  (
                    ContResult(
                      continuation,
                      persist,
                      channels,
                      patterns,
                      contSequenceNumber,
                      peeks.nonEmpty
                    ),
                    mats
                      .map(dc => Result(dc.channel, dc.datum.a, dc.removedDatum, dc.datum.persist))
                  )
                )
              }
      } yield r

    def getCommOrDataCandidates(comms: Seq[COMM]): F[Either[COMM, Seq[DataCandidate[C, A]]]] = {
      type COMMOrData = Either[COMM, Seq[DataCandidate[C, A]]]
      def go(comms: Seq[COMM]): F[Either[Seq[COMM], Either[COMM, Seq[DataCandidate[C, A]]]]] =
        comms match {
          case Nil =>
            val msg = "List comms must not be empty"
            logger.error(msg)
            Sync[F].raiseError(new IllegalArgumentException(msg))
          case commRef :: Nil =>
            runMatcher(commRef).map {
              case Some(x) => x.asRight[COMM].asRight[Seq[COMM]]
              case None    => commRef.asLeft[Seq[DataCandidate[C, A]]].asRight[Seq[COMM]]
            }
          case commRef :: rem =>
            runMatcher(commRef).map {
              case Some(x) => x.asRight[COMM].asRight[Seq[COMM]]
              case None    => rem.asLeft[COMMOrData]
            }
        }
      comms.tailRecM(go)
    }

    for {
      _ <- logF.debug(s"""|consume: searching for data matching <patterns: $patterns>
                          |at <channels: $channels>""".stripMargin.replace('\n', ' '))
      consumeRef <- syncF.delay {
                     Consume.create(channels, patterns, continuation, persist, sequenceNumber)
                   }
      reportingConsume = ReportingConsume(channels, patterns, continuation, peeks.toSeq)
      _                <- Sync[F].delay(report.update(s => s :+ reportingConsume))
      r <- replayData.get(consumeRef) match {
            case None =>
              storeWaitingContinuation(
                channels,
                WaitingContinuation(patterns, continuation, persist, peeks, consumeRef)
              )
            case Some(comms) =>
              val commOrDataCandidates: F[Either[COMM, Seq[DataCandidate[C, A]]]] =
                getCommOrDataCandidates(comms.iterator().asScala.toList)

              val x: F[MaybeActionResult] = commOrDataCandidates.flatMap {
                case Left(_) =>
                  storeWaitingContinuation(
                    channels,
                    WaitingContinuation(patterns, continuation, persist, peeks, consumeRef)
                  )
                case Right(dataCandidates) =>
                  val channelsToIndex = channels.zipWithIndex.toMap
                  val produces        = dataCandidates.map(dc => ReportingProduce(dc.channel, dc.datum.a))
                  Sync[F].delay(
                    report.update(
                      s => s :+ ReportingComm(consume = reportingConsume, produces = produces)
                    )
                  ) >>
                    handleMatches(
                      dataCandidates,
                      consumeRef,
                      comms,
                      peeks,
                      channelsToIndex
                    )
              }
              x
          }
    } yield r
  }
  def produce(channel: C, data: A, persist: Boolean, sequenceNumber: Int): F[MaybeActionResult] =
    (for {
      result <- produceLockF(channel) {
                 lockedProduce(channel, data, persist, sequenceNumber)
               }
    } yield result)

  private[this] def lockedProduce(
      channel: C,
      data: A,
      persist: Boolean,
      sequenceNumber: Int
  ): F[MaybeActionResult] = {

    type MaybeProduceCandidate = Option[ProduceCandidate[C, P, A, K]]

    def runMatcher(
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
                                           (for {
                                             data <- store.getData(c)
                                           } yield (data.zipWithIndex.filter {
                                             case (Datum(_, _, source), _) =>
                                               comm.produces.contains(source)
                                           })).map(
                                             as =>
                                               c -> {
                                                 if (c == channel)
                                                   Seq((Datum(data, persist, produceRef), -1))
                                                 else as
                                               }
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

    def getCommOrProduceCandidate(
        comms: Seq[COMM],
        produceRef: Produce,
        groupedChannels: Seq[Seq[C]]
    ): F[Either[COMM, ProduceCandidate[C, P, A, K]]] = {
      type COMMOrProduce = Either[COMM, ProduceCandidate[C, P, A, K]]
      def go(comms: Seq[COMM]): F[Either[Seq[COMM], COMMOrProduce]] =
        comms match {
          case Nil =>
            val msg = "comms must not be empty"
            logger.error(msg)
            Sync[F].raiseError(new IllegalArgumentException(msg))
          case commRef :: Nil =>
            runMatcher(commRef, produceRef, groupedChannels) map {
              case Some(x) => x.asRight[COMM].asRight[Seq[COMM]]
              case None    => commRef.asLeft[ProduceCandidate[C, P, A, K]].asRight[Seq[COMM]]
            }
          case commRef :: rem =>
            runMatcher(commRef, produceRef, groupedChannels) map {
              case Some(x) => x.asRight[COMM].asRight[Seq[COMM]]
              case None    => rem.asLeft[COMMOrProduce]
            }
        }
      comms.tailRecM(go)
    }

    def storeDatum(
        produceRef: Produce,
        maybeCommRef: Option[COMM]
    ): F[MaybeActionResult] =
      for {
        _ <- store.putDatum(channel, Datum(data, persist, produceRef))
        _ <- logF.debug(s"""|produce: no matching continuation found
                            |storing <data: $data> at <channel: $channel>""".stripMargin)
      } yield None

    def handleMatch(
        mat: ProduceCandidate[C, P, A, K],
        produceRef: Produce,
        comms: Multiset[COMM],
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
            _ <- metricsF.incrementCounter(produceCommLabel)
            commRef <- syncF.delay {
                        COMM(consumeRef, dataCandidates.map(_.datum.source), peeks)
                      }
            _                 <- assertF(comms.contains(commRef), "COMM Event was not contained in the trace")
            reportingConsume  = ReportingConsume(channels, patterns, continuation, peeks.toSeq)
            reportingProduces = dataCandidates.map(dc => ReportingProduce(dc.channel, dc.datum.a))
            _ <- Sync[F].delay(
                  report.update(s => s :+ ReportingComm(reportingConsume, reportingProduces))
                )
            _ <- if (!persistK) {
                  store.removeContinuation(channels, continuationIndex)
                } else {
                  ().pure[F]
                }
            _ <- dataCandidates.toList
                  .sortBy(_.datumIndex)(Ordering[Int].reverse)
                  .traverse {
                    case DataCandidate(candidateChannel, Datum(_, persistData, _), _, dataIndex) =>
                      (if (dataIndex >= 0 && !persistData) {
                         store.removeDatum(candidateChannel, dataIndex)
                       } else Applicative[F].unit) >>
                        store.removeJoin(candidateChannel, channels)
                  }
            _ <- logF.debug(s"produce: matching continuation found at <channels: $channels>")
            _ <- removeBindingsFor(commRef)
            r <- syncF.delay {
                  val contSequenceNumber = commRef.nextSequenceNumber
                  Some(
                    (
                      ContResult(
                        continuation,
                        persistK,
                        channels,
                        patterns,
                        contSequenceNumber,
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

    for {
      groupedChannels <- store.getJoins(channel)
      _ <- logF.debug(
            s"""|produce: searching for matching continuations
            |at <groupedChannels: $groupedChannels>""".stripMargin
              .replace('\n', ' ')
          )
      produceRef <- syncF.delay { Produce.create(channel, data, persist, sequenceNumber) }
      _          <- Sync[F].delay(report.update(s => s :+ ReportingProduce(channel, data)))
      result <- replayData.get(produceRef) match {
                 case None =>
                   storeDatum(produceRef, None)
                 case Some(comms) =>
                   val commOrProduceCandidate: F[Either[COMM, ProduceCandidate[C, P, A, K]]] =
                     getCommOrProduceCandidate(
                       comms.iterator().asScala.toList,
                       produceRef,
                       groupedChannels
                     )
                   val r: F[MaybeActionResult] = commOrProduceCandidate.flatMap {
                     case Left(comm) =>
                       storeDatum(produceRef, Some(comm))
                     case Right(produceCandidate) =>
                       val indexedChannels = produceCandidate.channels.zipWithIndex.toMap
                       handleMatch(produceCandidate, produceRef, comms, indexedChannels)
                   }
                   r
               }
    } yield result
  }

  private def assertF(predicate: Boolean, errorMsg: String): F[Unit] =
    if (!predicate)
      Sync[F].raiseError(
        new IllegalStateException(errorMsg)
      )
    else ().pure[F]

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  private def removeBindingsFor(
      commRef: COMM
  ): F[Unit] = Sync[F].delay {
    commRef.produces.foldLeft(replayData.removeBinding(commRef.consume, commRef)) {
      case (updatedReplays, produceRef) =>
        updatedReplays.removeBinding(produceRef, commRef)
    }
  }
}
