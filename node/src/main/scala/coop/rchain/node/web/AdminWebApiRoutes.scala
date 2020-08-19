package coop.rchain.node.web

import cats.effect.Sync
import cats.syntax.all._
import cats.~>
import coop.rchain.node.api.AdminWebApi
import org.http4s.{HttpRoutes, Response}

object AdminWebApiRoutes {

  def service[F[_]: Sync, M[_]: Sync](
      adminWebApi: AdminWebApi[M]
  )(implicit mf: M ~> F): HttpRoutes[F] = {
    import io.circe.syntax._
    import org.http4s.EntityEncoder
    import org.http4s.circe._

    val dsl = org.http4s.dsl.Http4sDsl[F]
    import dsl._

    implicit class MEx[A](val ma: M[A]) {
      def handle(implicit encoder: EntityEncoder[F, A]): F[Response[F]] =
        mf(ma)
          .flatMap(Ok(_))
          .handleErrorWith(err => BadRequest(err.getMessage.asJson))
    }

    // Encoders
    implicit val stringEncoder = jsonEncoderOf[F, String]

    HttpRoutes.of[F] {
      case POST -> Root / "propose" =>
        adminWebApi.propose.handle
    }
  }
}
