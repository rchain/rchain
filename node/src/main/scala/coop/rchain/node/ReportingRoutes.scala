package coop.rchain.node

import cats.implicits._
import cats.effect.Sync
import cats.{~>, Applicative}
import com.google.protobuf.ByteString
import coop.rchain.casper.ReportingCasper
import coop.rchain.crypto.codec.Base16
import coop.rchain.models.BlockHash._
import org.http4s.{HttpRoutes, QueryParamDecoder}

object ReportingRoutes {
  def service[F[_]: Sync, M[_]: Applicative](
      reportingCasper: ReportingCasper[M]
  )(implicit nt: M ~> F): HttpRoutes[F] = {
    val dsl = org.http4s.dsl.Http4sDsl[F]
    import dsl._
    import io.circe.syntax._
    import io.circe.generic.extras.auto._
    import io.circe.generic.extras.Configuration
    import org.http4s.circe.CirceEntityEncoder._

    implicit val genDevConfig: Configuration =
      Configuration.default
        .withDiscriminator("type")
        .withKebabCaseConstructorNames
        .withKebabCaseMemberNames

    implicit val BlockHashQueryParamDecoder: QueryParamDecoder[BlockHash] =
      QueryParamDecoder[String].map(s => ByteString.copyFrom(Base16.decode(s).get))

    object BlockHashQueryParamMatcher extends QueryParamDecoderMatcher[ByteString]("blockHash")

    HttpRoutes.of[F] {
      case GET -> Root / "trace" :? BlockHashQueryParamMatcher(hash) =>
        Ok { nt(reportingCasper.trace(hash).map(_.asJson)) }
    }
  }
}
