package coop.rchain.node

import cats.effect.Sync
import cats.~>
import com.google.protobuf.ByteString
import coop.rchain.casper.ReportingCasper
import coop.rchain.models.BlockHash._
import org.http4s.{HttpRoutes, QueryParamDecoder}

object ReportingRoutes {
  def service[F[_]: Sync, M[_]](
      reportingCasper: ReportingCasper[M]
  )(implicit nt: M ~> F): HttpRoutes[F] = {
    val dsl = org.http4s.dsl.Http4sDsl[F]
    import dsl._

    implicit val BlockHashQueryParamDecoder: QueryParamDecoder[BlockHash] =
      QueryParamDecoder[String].map(s => ByteString.copyFromUtf8(s))

    object BlockHashQueryParamMatcher extends QueryParamDecoderMatcher[ByteString]("blockHash")

    HttpRoutes.of[F] {
      case GET -> Root / "trace" :? BlockHashQueryParamMatcher(hash) =>
        Ok { nt(reportingCasper.trace(hash)) }
    }
  }
}
