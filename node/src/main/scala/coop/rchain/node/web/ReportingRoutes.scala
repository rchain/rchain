package coop.rchain.node.web

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.casper.api.BlockApi.ApiErr
import coop.rchain.casper.api.BlockReportApi
import coop.rchain.casper.protocol.BlockEventInfo
import org.http4s.{HttpRoutes, ParseFailure, QueryParamDecoder}
import org.http4s.circe.jsonEncoderOf
import coop.rchain.rspace.hashing.Blake2b256Hash

object ReportingRoutes {

  sealed trait ReportResponse
  final case class BlockTracesReport(
      report: BlockEventInfo
  ) extends ReportResponse
  final case class BlockReportError(errorMessage: String) extends ReportResponse

  type ReportingHttpRoutes[F[_]] = HttpRoutes[F]

  def transforResult[F[_]: Sync](
      hash: Blake2b256Hash,
      state: F[ApiErr[BlockEventInfo]]
  ): F[ReportResponse] =
    state.map(_.fold(BlockReportError, BlockTracesReport))

  def service[F[_]: Concurrent](
      blockReportAPI: BlockReportApi[F]
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

    implicit val blake2b256HashParamDecoder =
      QueryParamDecoder.stringQueryParamDecoder.emap(
        s => Blake2b256Hash.fromHexEither(s).leftMap(error => ParseFailure(error, ""))
      )
    object BlockHashParam   extends QueryParamDecoderMatcher[Blake2b256Hash]("blockHash")
    object ForceReplayParam extends OptionalQueryParamDecoderMatcher[Boolean]("forceReplay")
    implicit val encodeReportResponse = jsonEncoderOf[F, ReportResponse]

    HttpRoutes.of[F] {
      case GET -> Root / "trace" :? BlockHashParam(hash) +& ForceReplayParam(forceReplay) =>
        Ok(
          transforResult(
            hash,
            blockReportAPI.blockReport(hash.toByteString, forceReplay.getOrElse(false))
          )
        )
    }
  }
}
