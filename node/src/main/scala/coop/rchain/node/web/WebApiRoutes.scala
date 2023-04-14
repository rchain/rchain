package coop.rchain.node.web

import cats.effect.{Async, Sync}
import cats.syntax.all._
import coop.rchain.node.api.WebApi
import coop.rchain.node.api.WebApi._
import coop.rchain.node.encode.JsonEncoder._
import coop.rchain.sdk.syntax.all._
import coop.rchain.shared.Log
import io.circe.generic.semiauto._
import org.http4s.{HttpRoutes, Response}

object WebApiRoutes {

  def service[F[_]: Async: Log](webApi: WebApi[F]): HttpRoutes[F] = {
    import coop.rchain.casper.protocol.{BlockInfo, LightBlockInfo}
    import io.circe._
    import io.circe.generic.auto._
    import io.circe.syntax._
    import org.http4s.circe._
    import org.http4s.{EntityDecoder, EntityEncoder, InvalidMessageBodyFailure, Request}

    val dsl = org.http4s.dsl.Http4sDsl[F]
    import dsl._

    implicit class ResponseErrorHandler(val res: F[Response[F]]) {
      def handleResponseError =
        res
          .handleErrorWith {
            // The place where all API errors are handled
            // TODO: introduce error codes
            case err: InvalidMessageBodyFailure =>
              // Request JSON parse errors
              BadRequest(s"${err.getMessageSafe.take(250)}...".asJson)
            // Errors from BlockAPI
            case err: BlockApiException => BadRequest(err.getMessageSafe.asJson)
            case err: Throwable         =>
              // Logging only unanticipated errors, not related to Block API or input parsing (user errors)
              // To indicate that error is not related to internal node error and
              // node can continue running, error logged as a warning.
              Log[F].warn("HTTP API response error", err) *> BadRequest(err.getMessageSafe.asJson)
          }
    }

    def handleRequest[A, B](req: Request[F], f: A => F[B])(
        implicit decoder: EntityDecoder[F, A],
        encoder: EntityEncoder[F, B]
    ): F[Response[F]] =
      req
        .attemptAs[A]
        .value
        .flatMap(_.liftTo[F])
        .flatMap(f)
        .flatMap(Ok(_))
        .handleResponseError

    implicit class MEx[A](val ma: F[A]) {
      // Handle GET requests
      //   case GET -> Root / "last-finalized-block" =>
      //     webApi.lastFinalizedBlock.handle
      def handle(implicit encoder: EntityEncoder[F, A]): F[Response[F]] =
        ma.flatMap(Ok(_)).handleResponseError
    }

    implicit class RequestEx(val req: Request[F]) {
      // Handle POST requests
      //   case req @ POST -> Root / "deploy" =>
      //     req.handle[DeployRequest, String](webApi.deploy)
      def handle[A, B](
          f: A => F[B]
      )(implicit decoder: EntityDecoder[F, A], encoder: EntityEncoder[F, B]): F[Response[F]] =
        handleRequest[A, B](req, f)

      // Handle POST requests without input parameters
      //   case req @ POST -> Root / "last-finalized-block" =>
      //     req.handle_(webApi.lastFinalizedBlock)
      def handle_[B](f: F[B])(implicit encoder: EntityEncoder[F, B]): F[Response[F]] =
        handleRequest[Unit, B](req, _ => f)
    }

    // TODO: Create generic encoders/decoders for
    // ADT's with discriminator field
    implicit val encodeLightBlockInfo: Encoder[LightBlockInfo] = deriveEncoder[LightBlockInfo]

    implicit val encodeBlockInfo: Encoder[BlockInfo] = deriveEncoder[BlockInfo]

    // Encoders
    implicit val stringEncoder              = jsonEncoderOf[F, String]
    implicit val booleanEncode              = jsonEncoderOf[F, Boolean]
    implicit val apiStatusEncoder           = jsonEncoderOf[F, ApiStatus]
    implicit val blockInfoEncoder           = jsonEncoderOf[F, BlockInfo]
    implicit val lightBlockEncoder          = jsonEncoderOf[F, LightBlockInfo]
    implicit val lightBlockListEnc          = jsonEncoderOf[F, List[LightBlockInfo]]
    implicit val dataAtNameRespEncoder      = jsonEncoderOf[F, DataAtNameResponse]
    implicit val dataAtParRespEncoder       = jsonEncoderOf[F, RhoDataResponse]
    implicit val transactionResponseEncoder = jsonEncoderOf[F, TransactionResponse]
    // Decoders
    implicit val deployRequestDecoder     = jsonOf[F, DeployRequest]
    implicit val dataAtNameRequestDecoder = jsonOf[F, DataAtNameRequest]
    implicit val dataAtParRequestDecoder  = jsonOf[F, DataAtNameByBlockHashRequest]
    implicit val ExploreDeployRequest     = jsonOf[F, ExploreDeployRequest]

    HttpRoutes.of[F] {
      case GET -> Root / "status" =>
        webApi.status.handle

      // Deploy

      case req @ POST -> Root / "deploy" =>
        req.handle[DeployRequest, String](webApi.deploy)

      case req @ POST -> Root / "explore-deploy" =>
        req.handle[String, RhoDataResponse](
          term => webApi.exploratoryDeploy(term, none[String], usePreStateHash = false)
        )

      case req @ POST -> Root / "explore-deploy-by-block-hash" =>
        req.handle[ExploreDeployRequest, RhoDataResponse](
          req =>
            if (req.blockHash.isEmpty)
              webApi.exploratoryDeploy(req.term, none[String], req.usePreStateHash)
            else
              webApi.exploratoryDeploy(req.term, Some(req.blockHash), req.usePreStateHash)
        )

      // Get data

      case req @ POST -> Root / "data-at-name" =>
        req.handle[DataAtNameRequest, DataAtNameResponse](webApi.listenForDataAtName)

      case req @ POST -> Root / "data-at-name-by-block-hash" =>
        req.handle[DataAtNameByBlockHashRequest, RhoDataResponse](webApi.getDataAtPar)

      // Blocks

      case GET -> Root / "last-finalized-block" =>
        webApi.lastFinalizedBlock.handle

      case GET -> Root / "block" / hash =>
        webApi.getBlock(hash).handle

      case GET -> Root / "blocks" =>
        webApi.getBlocks(1).handle

      case GET -> Root / "blocks" / IntVar(startBlockNumber) / IntVar(endBlockNumber) =>
        webApi.getBlocksByHeights(startBlockNumber.toLong, endBlockNumber.toLong).handle

      case GET -> Root / "blocks" / IntVar(depth) =>
        webApi.getBlocks(depth).handle

      case GET -> Root / "deploy" / deployId =>
        webApi.findDeploy(deployId).handle

      case GET -> Root / "is-finalized" / hash =>
        webApi.isFinalized(hash).handle

      case GET -> Root / "transactions" / hash =>
        webApi.getTransaction(hash).handle
    }
  }

}
