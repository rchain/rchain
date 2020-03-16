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
import coop.rchain.shared.{Log, Serialize}
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

import scala.concurrent.{ExecutionContext, SyncVar}

/**
  * ReportingRspace works exactly like how ReplayRspace works. It can replay the deploy and try to find if the
  * deploy can be replayed well. But instead of just replaying the deploy, the ReportingRspace also save the comm
  * event data into the `report` val.
  *
  * Currently only the unmatched comm event data are left in the tuplespace which means that the comm event data
  * happened in the processing of the deploy does not save anywhere in the software. It is believed that if we save
  * every comm event data during processing the deploy, the execution of Rholang would be much slower. But this(not
  * saving all comm event data) also leads to another problem that a developer can not get history data of deploy which
  * some of the comm event data are important to them. This ReportingRspace is trying to address this issue and let
  * people get the comm event data from replay.
  */
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
    m: Match[F, P, A],
    concurrent: Concurrent[F],
    logF: Log[F],
    contextShift: ContextShift[F],
    scheduler: ExecutionContext,
    metricsF: Metrics[F],
    spanF: Span[F]
) extends ReplayRSpace[F, C, P, A, K](historyRepository, storeAtom, branch) {

  protected[this] override val logger: Logger = Logger[this.type]

  implicit protected[this] override lazy val MetricsSource: Metrics.Source =
    Metrics.Source(RSpaceMetricsSource, "reporting")

  val report: SyncVar[Seq[ReportingEvent]] = create[Seq[ReportingEvent]](Seq.empty)

  def getReport: F[Seq[ReportingEvent]] = Sync[F].delay(report.get)

  protected override def logComm(
      dataCandidates: Seq[ConsumeCandidate[C, A]],
      channels: Seq[C],
      wk: WaitingContinuation[P, K],
      comm: COMM,
      label: String
  ): F[COMM] =
    for {
      commRef           <- super.logComm(dataCandidates, channels, wk, comm, label)
      reportingConsume  = ReportingConsume(channels, wk.patterns, wk.continuation, wk.peeks.toSeq)
      reportingProduces = dataCandidates.map(dc => ReportingProduce(dc.channel, dc.datum.a))
      _ <- Sync[F].delay(
            report.update(s => s :+ ReportingComm(reportingConsume, reportingProduces))
          )
    } yield commRef

  protected override def logConsume(
      consumeRef: Consume,
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      peeks: SortedSet[Int]
  ): F[Consume] =
    for {
      _ <- super.logConsume(
            consumeRef,
            channels,
            patterns,
            continuation,
            persist,
            peeks
          )
      reportingConsume = ReportingConsume(channels, patterns, continuation, peeks.toSeq)
      _                <- Sync[F].delay(report.update(s => s :+ reportingConsume))
    } yield consumeRef

  protected override def logProduce(
      produceRef: Produce,
      channel: C,
      data: A,
      persist: Boolean
  ): F[Produce] =
    for {
      _ <- super.logProduce(produceRef, channel, data, persist)
      _ <- Sync[F].delay(report.update(s => s :+ ReportingProduce(channel, data)))
    } yield produceRef

  /** ReportingCasper would reset(empty) the report data in every createCheckpoint.
    *
    */
  override def createCheckpoint(): F[Checkpoint] = checkReplayData >> syncF.defer {
    val historyRepository = historyRepositoryAtom.get()
    for {
      _ <- createNewHotStore(historyRepository)(serializeK.toCodec)
      _ <- restoreInstalls()
      _ = report.update(_ => Seq.empty[ReportingEvent])
    } yield (Checkpoint(historyRepository.history.root, Seq.empty))
  }

}
