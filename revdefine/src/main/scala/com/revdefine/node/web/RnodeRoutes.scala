package com.revdefine.node.web

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Par
import org.http4s.{EntityDecoder, EntityEncoder, HttpRoutes}
import org.http4s.circe.{jsonEncoderOf, jsonOf}
import io.circe.generic.auto._
import com.revdefine.node.web.node.api.API
import com.revdefine.node.web.node.models._

object RnodeRoutes {

  final case class ExploreDeployReq(
      term: String,
      blockHash: String,
      usePreStateHash: Boolean
  )

  def service[F[_]: Sync](api: API[F]): HttpRoutes[F] = {

    import coop.rchain.node.encode.JsonEncoder._
    val dsl = org.http4s.dsl.Http4sDsl[F]
    import dsl._

    implicit val ExploreDeployReqDecoder: EntityDecoder[F, ExploreDeployReq] =
      jsonOf[F, ExploreDeployReq]
    implicit val BlockHeaderEncoder: EntityEncoder[F, BlockHeader] = jsonEncoderOf[F, BlockHeader]
    implicit val BooleanEncoder: EntityEncoder[F, Boolean]         = jsonEncoderOf[F, Boolean]
    implicit val BlockEncoder: EntityEncoder[F, Block]             = jsonEncoderOf[F, Block]
    implicit val parEncoder: EntityEncoder[F, Seq[Par]]            = jsonEncoderOf[F, Seq[Par]]
    implicit val BlockHeaderVectorEncoder: EntityEncoder[F, Vector[BlockHeader]] =
      jsonEncoderOf[F, Vector[BlockHeader]]

    HttpRoutes.of[F] {

      case req @ POST -> Root / "explore-deploy-by-block-hash" =>
        req.as[ExploreDeployReq] >>= { r =>
          api.exploreDeploy(r.term, r.blockHash, r.usePreStateHash)
        } >>= { r =>
          Ok(r)
        }

      // Blocks
      case GET -> Root / "last-finalized-block" =>
        api.lastFinalizedBlock.map(convertBlockHeader).flatMap(Ok(_))

      case GET -> Root / "block" / hash =>
        api.getBlock(hash).map(convertBlock).flatMap(Ok(_))

      case GET -> Root / "blocks" =>
        api.getBlocks(1).map(_.map(convertBlockHeader)).flatMap(Ok(_))

      case GET -> Root / "blocks" / IntVar(startBlockNumber) / IntVar(endBlockNumber) =>
        api
          .getBlocksByHeights(startBlockNumber.toLong, endBlockNumber.toLong)
          .map(_.map(convertBlockHeader))
          .flatMap(Ok(_))

      case GET -> Root / "blocks" / IntVar(depth) =>
        api.getBlocks(depth).map(_.map(convertBlockHeader)).flatMap(Ok(_))

      case GET -> Root / "deploy" / deployId =>
        api.findDeploy(deployId).map(convertBlockHeader).flatMap(Ok(_))

      case GET -> Root / "is-finalized" / hash =>
        api.isFinalized(hash).flatMap(Ok(_))
    }
  }

}
