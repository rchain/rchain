package coop.rchain.node.web

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.casper.api.BlockAPI.ApiErr
import coop.rchain.casper.api.BlockReportAPI
import coop.rchain.casper.protocol.BlockEventInfo
import org.http4s.HttpRoutes
import org.http4s.circe.jsonEncoderOf
import coop.rchain.models.syntax._

object ReportingRoutes {

  sealed trait ReportResponse
  final case class BlockTracesReport(
      report: BlockEventInfo
  ) extends ReportResponse
  final case class BlockReportError(hash: String, errorMessage: String) extends ReportResponse

  type ReportingHttpRoutes[F[_]] = HttpRoutes[F]

  def transforResult[F[_]: Sync](
      hash: String,
      state: F[ApiErr[BlockEventInfo]]
  ): F[ReportResponse] =
    state.map(_.fold(BlockReportError(hash, _), BlockTracesReport))

  def service[F[_]: Concurrent](
      blockReportAPI: BlockReportAPI[F]
  ): ReportingHttpRoutes[F] = {
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

    object BlockHashParam   extends QueryParamDecoderMatcher[String]("blockHash")
    object ForceReplayParam extends OptionalQueryParamDecoderMatcher[Boolean]("forceReplay")
    implicit val encodeReportResponse = jsonEncoderOf[F, ReportResponse]

    HttpRoutes.of[F] {
      case GET -> Root / "trace" :? BlockHashParam(hash) +& ForceReplayParam(forceReplay) =>
        Ok(transforResult(hash, blockReportAPI.blockReport(hash, forceReplay.getOrElse(false))))
    }
  }
}
