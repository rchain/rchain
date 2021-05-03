package coop.rchain.node.web

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.api.BlockAPI.ApiErr
import coop.rchain.casper.api.BlockReportAPI
import coop.rchain.casper.{ReportStore, ReportingCasper, SafetyOracle}
import coop.rchain.models.BlockHash._
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.casper.protocol.BlockEventInfo
import coop.rchain.metrics.{Metrics, MetricsSemaphore, Span}
import org.http4s.HttpRoutes
import coop.rchain.shared.Log
import org.http4s.circe.jsonEncoderOf

import scala.collection.concurrent.TrieMap

object ReportingRoutes {

  sealed trait ReportResponse
  final case class BlockTracesReport(
      report: BlockEventInfo
  ) extends ReportResponse
  final case class BlockReportError(hash: String, errorMessage: String) extends ReportResponse

  type ReportingHTTPRoutes[F[_]] = HttpRoutes[F]

  def transforResult[F[_]: Sync](
      hash: String,
      state: F[ApiErr[BlockEventInfo]]
  ): F[ReportResponse] =
    state.map(
      s =>
        s match {
          case Left(error) => BlockReportError(hash, error)
          case Right(b)    => BlockTracesReport(b)
        }
    )

  def service[F[_]: Concurrent](
      blockReportAPI: BlockReportAPI[F]
  ): ReportingHTTPRoutes[F] = {
    val dsl = org.http4s.dsl.Http4sDsl[F]
    import dsl._
    import io.circe.generic.extras.Configuration
    import io.circe.generic.extras.auto._
    import coop.rchain.node.encode.JsonEncoder._

    implicit val genDevConfig: Configuration =
      Configuration.default
        .withDiscriminator("type")
        .withKebabCaseConstructorNames
        .withKebabCaseMemberNames

    object BlockHashQueryParamMatcher extends QueryParamDecoderMatcher[String]("blockHash")
    implicit val encodeReportResponse = jsonEncoderOf[F, ReportResponse]

    HttpRoutes.of[F] {
      case GET -> Root / "trace" :? BlockHashQueryParamMatcher(hash) =>
        Ok { transforResult(hash, blockReportAPI.blockReport(hash, false)) }
    }
  }
}
