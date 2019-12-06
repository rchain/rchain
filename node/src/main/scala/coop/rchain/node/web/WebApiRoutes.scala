package coop.rchain.node.web

import cats.effect.Sync
import cats.syntax.all._
import cats.~>
import coop.rchain.node.api.WebApi
import coop.rchain.node.api.WebApi._
import org.http4s.{HttpRoutes, Response}

object WebApiRoutes {
  import WebApiSyntax._

  def service[F[_]: Sync, M[_]: Sync](
      webApi: WebApi[M]
  )(implicit mf: M ~> F): HttpRoutes[F] = {
    import io.circe.generic.auto._
    import io.circe.syntax._
    import org.http4s.circe._
    import org.http4s.{EntityDecoder, EntityEncoder, InvalidMessageBodyFailure, Request}

    val dsl = org.http4s.dsl.Http4sDsl[F]
    import dsl._

    def handleRequest[A, B](req: Request[F], f: A => M[B])(
        implicit decoder: EntityDecoder[F, A],
        encoder: EntityEncoder[F, B]
    ): F[Response[F]] =
      req
        .attemptAs[A]
        .value
        .flatMap(_.leftToError[F])
        .flatMap(a => mf(f(a)))
        .flatMap(Ok(_))
        .handleErrorWith {
          // The place where all API errors are handled
          // TODO: introduce error codes
          // Request JSON parse errors
          case err: InvalidMessageBodyFailure =>
            BadRequest(s"${err.getMessage.take(250)}...".asJson)
          // Errors from BlockAPI
          case err: BlockApiException => BadRequest(err.getMessage.asJson)
          case err: Throwable         => BadRequest(err.getMessage.asJson)
        }

    implicit class MEx[A](val ma: M[A]) {
      // Handle GET requests
      //   case GET -> Root / "lastFinalizedBlock" =>
      //     webApi.lastFinalizedBlock.handle
      def handle(implicit encoder: EntityEncoder[F, A]): F[Response[F]] =
        mf(ma)
          .flatMap(Ok(_))
          .handleErrorWith(err => BadRequest(err.getMessage.asJson))
    }

    implicit class RequestEx(val req: Request[F]) {
      // Handle POST requests
      //   case req @ POST -> Root / "deploy" =>
      //     req.handle[SignedDeploy, String](WebApi.toSigned[M](_) >>= webApi.deploy)
      def handle[A, B](
          f: A => M[B]
      )(implicit decoder: EntityDecoder[F, A], encoder: EntityEncoder[F, B]): F[Response[F]] =
        handleRequest[A, B](req, f)

      // Handle POST requests without input parameters
      //   case req @ POST -> Root / "lastFinalizedBlock" =>
      //     req.handle_(webApi.lastFinalizedBlock)
      def handle_[B](
          f: M[B]
      )(implicit encoder: EntityEncoder[F, B]): F[Response[F]] =
        handleRequest[Unit, B](req, _ => f)
    }

    // TODO: Create generic encoders/decoders for
    // ADT's with discriminator field

    // Encoders
    implicit val stringEncoder     = jsonEncoderOf[F, String]
    implicit val apiStatusEncoder  = jsonEncoderOf[F, ApiStatus]
    implicit val blockInfoEncoder  = jsonEncoderOf[F, BlockInfo]
    implicit val lightBlockEncoder = jsonEncoderOf[F, List[LightBlockInfo]]
    implicit val dataRespEncoder   = jsonEncoderOf[F, DataResponse]
    implicit val prepareEncoder    = jsonEncoderOf[F, PrepareResponse]
    // Decoders
    implicit val signedDeployDecoder = jsonOf[F, SignedDeploy]
    implicit val dataRequestDecoder  = jsonOf[F, DataRequest]
    implicit val prepareDecoder      = jsonOf[F, PrepareRequest]

    HttpRoutes.of[F] {
      case GET -> Root / "status" =>
        webApi.status.handle

      // Prepare deploy

      case GET -> Root / "prepare-deploy" =>
        webApi.prepareDeploy(none).handle

      case req @ POST -> Root / "prepare-deploy" =>
        req.handle[PrepareRequest, PrepareResponse](x => webApi.prepareDeploy(x.some))

      // Deploy

      case req @ POST -> Root / "deploy" =>
        req.handle[SignedDeploy, String](WebApi.toSigned[M](_) >>= webApi.deploy)

      // Get data

      case req @ POST -> Root / "data-at-name" =>
        req.handle[DataRequest, DataResponse](webApi.listenForDataAtName)

      // Blocks

      case GET -> Root / "last-finalized-block" =>
        webApi.lastFinalizedBlock.handle

      case GET -> Root / "block" / hash =>
        webApi.getBlock(hash).handle

      case GET -> Root / "blocks" =>
        webApi.getBlocks(none).handle

      case GET -> Root / "blocks" / IntVar(depth) =>
        webApi.getBlocks(depth.some).handle
    }
  }

}
