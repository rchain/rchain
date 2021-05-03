package coop.rchain.casper.api

import cats.Monad
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.{
  CasperMetricsSource,
  DeployReportResult,
  MultiParentCasper,
  ReportError,
  ReportReplayError,
  ReportStore,
  ReportingCasper,
  SafetyOracle,
  SystemDeployReportResult
}
import coop.rchain.casper.api.BlockAPI.{getLightBlockInfo, reportTransformer, ApiErr, Error}
import coop.rchain.casper.engine.EngineCell
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.casper.protocol.{
  BlockEventInfo,
  BlockMessage,
  DeployInfoWithEventData,
  ReportCommProto,
  ReportConsumeProto,
  ReportProduceProto,
  ReportProto,
  SingleReport,
  SystemDeployData,
  SystemDeployInfoWithEventData
}
import coop.rchain.crypto.codec.Base16
import coop.rchain.metrics.{Metrics, MetricsSemaphore, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.rspace.ReportingRspace.ReportingEvent
import coop.rchain.shared.Log

import scala.collection.concurrent.TrieMap

class BlockReportAPI[F[_]: Concurrent: Metrics: EngineCell: Log: SafetyOracle: BlockStore](
    reportingCasper: ReportingCasper[F],
    reportStore: ReportStore[F]
) {
  implicit val source                                       = Metrics.Source(CasperMetricsSource, "report-replay")
  val blockLockMap: TrieMap[BlockHash, MetricsSemaphore[F]] = TrieMap.empty
  private def replayBlock(
      b: BlockMessage
  )(implicit casper: MultiParentCasper[F]) =
    for {
      reportResult <- reportingCasper.trace(b)
      res <- reportResult match {
              case Left(ReportReplayError(r)) =>
                s"Block replayed error ${r}".asLeft[BlockEventInfo].pure[F]
              case Right(result) =>
                for {
                  lightBlock <- BlockAPI.getLightBlockInfo[F](b)
                  deploys = createDeployReport(
                    result.deployReportResult
                  )
                  sysDeploys = createSystemDeployReport(
                    result.systemDeployReportResult
                  )
                  blockEvent = BlockEventInfo(
                    Some(lightBlock),
                    deploys,
                    sysDeploys,
                    result.postStateHash
                  )
                } yield blockEvent.asRight[Error]
            }
    } yield res

  private def blockReportWithinLock(
      forceReplay: Boolean,
      b: BlockMessage
  )(implicit casper: MultiParentCasper[F]) =
    for {
      semaphore <- MetricsSemaphore.single
      lock = blockLockMap
        .getOrElseUpdate(b.blockHash, semaphore)
      result <- lock.withPermit(
                 for {
                   cached <- reportStore.get(b.blockHash)
                   res <- if (cached.isEmpty || forceReplay)
                           for {
                             blockEvent <- replayBlock(b)
                             _ <- blockEvent.fold(
                                   _ => ().pure[F],
                                   data => reportStore.put(b.blockHash, data)
                                 )
                           } yield blockEvent
                         else
                           cached.get
                             .asRight[Error]
                             .pure[F]
                 } yield res
               )
    } yield result

  def blockReport(
      hash: String,
      forceReplay: Boolean
  ): F[ApiErr[BlockEventInfo]] =
    for {
      engine <- EngineCell[F].read
      res <- engine.withCasper(
              casper => {
                implicit val c = casper
                for {
                  isReadOnly <- casper.getValidator
                  result <- isReadOnly match {
                             case None =>
                               for {
                                 maybeBlock <- BlockStore[F].get(
                                                ByteString.copyFrom(Base16.unsafeDecode(hash))
                                              )
                                 report <- maybeBlock match {
                                            case Some(b) =>
                                              blockReportWithinLock(forceReplay, b)
                                            case _ =>
                                              s"Block ${hash} not found"
                                                .asLeft[BlockEventInfo]
                                                .pure[F]
                                          }
                               } yield report
                             case Some(_) =>
                               "Block report can only be executed on read-only RNode."
                                 .asLeft[BlockEventInfo]
                                 .pure[F]
                           }
                } yield result
              },
              Log[F]
                .warn("Could not get event data.")
                .as("Error: Could not get event data.".asLeft[BlockEventInfo])
            )
    } yield res

  private def createSystemDeployReport(
      result: List[SystemDeployReportResult]
  ): List[SystemDeployInfoWithEventData] = result.map(
    sd =>
      SystemDeployInfoWithEventData(
        Some(SystemDeployData.toProto(sd.processedSystemDeploy)),
        sd.events
          .map(
            a =>
              SingleReport(events = a.map(reportTransformer.transformEvent(_) match {
                case rc: ReportConsumeProto => ReportProto(ReportProto.Report.Consume(rc))
                case rp: ReportProduceProto => ReportProto(ReportProto.Report.Produce(rp))
                case rcm: ReportCommProto   => ReportProto(ReportProto.Report.Comm(rcm))
              }))
          )
      )
  )

  private def createDeployReport(
      result: List[DeployReportResult]
  ): List[DeployInfoWithEventData] =
    result
      .map(
        p =>
          DeployInfoWithEventData(
            deployInfo = p.processedDeploy.toDeployInfo.some,
            report = p.events
              .map(
                a =>
                  SingleReport(events = a.map(reportTransformer.transformEvent(_) match {
                    case rc: ReportConsumeProto => ReportProto(ReportProto.Report.Consume(rc))
                    case rp: ReportProduceProto => ReportProto(ReportProto.Report.Produce(rp))
                    case rcm: ReportCommProto   => ReportProto(ReportProto.Report.Comm(rcm))
                  }))
              )
          )
      )

}

object BlockReportAPI {
  def instance[F[_]: Concurrent: Metrics: EngineCell: Log: SafetyOracle: BlockStore](
      reportingCasper: ReportingCasper[F],
      reportStore: ReportStore[F]
  ): BlockReportAPI[F] = new BlockReportAPI[F](reportingCasper, reportStore)
}
