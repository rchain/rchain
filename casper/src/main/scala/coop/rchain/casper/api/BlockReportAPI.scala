package coop.rchain.casper.api

import cats.data.EitherT
import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.ReportStore.ReportStore
import coop.rchain.casper.{
  CasperMetricsSource,
  DeployReportResult,
  MultiParentCasper,
  ReportingCasper,
  SafetyOracle,
  SystemDeployReportResult
}
import coop.rchain.casper.api.BlockAPI.{reportTransformer, ApiErr, Error}
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
import coop.rchain.metrics.{Metrics, MetricsSemaphore}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._

import scala.collection.concurrent.TrieMap

class BlockReportAPI[F[_]: Concurrent: Metrics: EngineCell: Log: SafetyOracle: BlockStore](
    reportingCasper: ReportingCasper[F],
    reportStore: ReportStore[F]
) {
  implicit val source                                       = Metrics.Source(CasperMetricsSource, "report-replay")
  val blockLockMap: TrieMap[BlockHash, MetricsSemaphore[F]] = TrieMap.empty

  private def replayBlock(b: BlockMessage)(implicit casper: MultiParentCasper[F]) =
    for {
      reportResult <- reportingCasper.trace(b)
      lightBlock   <- BlockAPI.getLightBlockInfo[F](b)
      deploys      = createDeployReport(reportResult.deployReportResult)
      sysDeploys   = createSystemDeployReport(reportResult.systemDeployReportResult)
      blockEvent   = BlockEventInfo(Some(lightBlock), deploys, sysDeploys, reportResult.postStateHash)
    } yield blockEvent

  private def blockReportWithinLock(forceReplay: Boolean, b: BlockMessage)(
      implicit casper: MultiParentCasper[F]
  ) =
    for {
      semaphore <- MetricsSemaphore.single
      lock      = blockLockMap.getOrElseUpdate(b.blockHash, semaphore)
      result <- lock.withPermit(
                 for {
                   cached <- reportStore.get(b.blockHash)
                   res <- if (cached.isEmpty || forceReplay)
                           replayBlock(b).flatTap(reportStore.put(b.blockHash, _))
                         else
                           cached.get.pure[F]
                 } yield res
               )
    } yield result

  def blockReport(hash: String, forceReplay: Boolean): F[ApiErr[BlockEventInfo]] = {
    def createReport(casper: MultiParentCasper[F]): F[Either[Error, BlockEventInfo]] = {
      implicit val c = casper
      for {
        maybeBlock <- BlockStore[F].get(ByteString.copyFrom(Base16.unsafeDecode(hash)))
        report     <- maybeBlock.traverse(blockReportWithinLock(forceReplay, _))
      } yield report.toRight(s"Block $hash not found")
    }

    def validateReadOnlyNode(casper: MultiParentCasper[F]): F[Either[Error, MultiParentCasper[F]]] =
      casper.getValidator.map { pkOpt =>
        // Error for validator node
        if (pkOpt.isEmpty) casper.asRight[Error]
        else "Block report can only be executed on read-only RNode.".asLeft[MultiParentCasper[F]]
      }

    // Process report if read-only node and block is found
    def processReport(casper: MultiParentCasper[F]): F[Either[Error, BlockEventInfo]] =
      EitherT(validateReadOnlyNode(casper)).flatMapF(createReport).value

    def casperNotInitialized: F[Either[Error, BlockEventInfo]] =
      Log[F]
        .warn("Could not get event data.")
        .as("Error: Could not get event data.".asLeft[BlockEventInfo])

    EngineCell[F].read.flatMap(_.withCasper(processReport, casperNotInitialized))
  }

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
  def apply[F[_]: Concurrent: Metrics: EngineCell: Log: SafetyOracle: BlockStore](
      reportingCasper: ReportingCasper[F],
      reportStore: ReportStore[F]
  ): BlockReportAPI[F] = new BlockReportAPI[F](reportingCasper, reportStore)
}
