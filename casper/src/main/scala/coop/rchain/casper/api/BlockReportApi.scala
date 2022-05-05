package coop.rchain.casper.api

import cats.data.EitherT
import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.casper._
import coop.rchain.casper.api.BlockApi._
import coop.rchain.casper.protocol._
import coop.rchain.casper.reporting.ReportStore.ReportStore
import coop.rchain.casper.reporting.{
  DeployReportResult,
  ReportingCasper,
  ReportingProtoTransformer,
  SystemDeployReportResult
}
import coop.rchain.metrics.{Metrics, MetricsSemaphore}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._

import scala.collection.concurrent.TrieMap

class BlockReportApi[F[_]: Concurrent: BlockStore: Metrics: Log](
    reportingCasper: ReportingCasper[F],
    reportStore: ReportStore[F],
    validatorIdentityOpt: Option[ValidatorIdentity]
) {
  implicit val source                                       = Metrics.Source(CasperMetricsSource, "report-replay")
  val blockLockMap: TrieMap[BlockHash, MetricsSemaphore[F]] = TrieMap.empty

  val reportTransformer = new ReportingProtoTransformer()

  private def replayBlock(b: BlockMessage) =
    for {
      reportResult <- reportingCasper.trace(b)
      lightBlock   = getLightBlockInfo(b)
      deploys      = createDeployReport(reportResult.deployReportResult)
      sysDeploys   = createSystemDeployReport(reportResult.systemDeployReportResult)
      blockEvent   = BlockEventInfo(lightBlock, deploys, sysDeploys, reportResult.postStateHash)
    } yield blockEvent

  private def blockReportWithinLock(forceReplay: Boolean, b: BlockMessage) =
    for {
      semaphore <- MetricsSemaphore.single
      lock      = blockLockMap.getOrElseUpdate(b.blockHash, semaphore)
      result <- lock.withPermit(
                 for {
                   cached <- reportStore.get1(b.blockHash)
                   res <- if (cached.isEmpty || forceReplay)
                           replayBlock(b).flatTap(reportStore.put(b.blockHash, _))
                         else
                           cached.get.pure[F]
                 } yield res
               )
    } yield result

  def blockReport(hash: BlockHash, forceReplay: Boolean): F[ApiErr[BlockEventInfo]] = {
    def createReport: F[Either[Error, BlockEventInfo]] =
      for {
        maybeBlock <- BlockStore[F].get1(hash)
        report     <- maybeBlock.traverse(blockReportWithinLock(forceReplay, _))
      } yield report.toRight(s"Block $hash not found")

    // Error if not read-only node (has validator private key)
    val readOnlyNode = validatorIdentityOpt
      .as("Block report can only be executed on read-only RNode.".asLeft)
      .getOrElse(().asRight)

    // Process report if read-only node and block is found
    (readOnlyNode.toEitherT[F] >> EitherT(createReport)).value
  }

  private def createSystemDeployReport(
      result: List[SystemDeployReportResult]
  ): List[SystemDeployInfoWithEventData] = result.map { sd =>
    SystemDeployInfoWithEventData(
      SystemDeployData.toProto(sd.processedSystemDeploy),
      sd.events.map { a =>
        SingleReport(events = a.map(reportTransformer.transformEvent(_) match {
          case rc: ReportConsumeProto => ReportProto(ReportProto.Report.Consume(rc))
          case rp: ReportProduceProto => ReportProto(ReportProto.Report.Produce(rp))
          case rcm: ReportCommProto   => ReportProto(ReportProto.Report.Comm(rcm))
        }))
      }
    )
  }

  private def createDeployReport(
      result: List[DeployReportResult]
  ): List[DeployInfoWithEventData] =
    result.map { p =>
      DeployInfoWithEventData(
        deployInfo = p.processedDeploy.toDeployInfo,
        report = p.events.map { a =>
          SingleReport(events = a.map(reportTransformer.transformEvent(_) match {
            case rc: ReportConsumeProto => ReportProto(ReportProto.Report.Consume(rc))
            case rp: ReportProduceProto => ReportProto(ReportProto.Report.Produce(rp))
            case rcm: ReportCommProto   => ReportProto(ReportProto.Report.Comm(rcm))
          }))
        }
      )
    }

}

object BlockReportApi {
  def apply[F[_]: Concurrent: BlockStore: Metrics: Log](
      reportingCasper: ReportingCasper[F],
      reportStore: ReportStore[F],
      validatorIdentityOpt: Option[ValidatorIdentity]
  ): BlockReportApi[F] = new BlockReportApi[F](reportingCasper, reportStore, validatorIdentityOpt)
}
