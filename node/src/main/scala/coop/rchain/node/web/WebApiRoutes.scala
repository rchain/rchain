package coop.rchain.node.web

import cats.effect.{Effect, Sync}
import cats.syntax.all._
import cats.~>
import com.google.protobuf.ByteString
import coop.rchain.casper.PrettyPrinter
import coop.rchain.node.api.WebApi
import coop.rchain.node.api.WebApi._
import coop.rchain.shared.Log
import io.circe.generic.semiauto.deriveEncoder
import org.http4s.{EntityDecoder, EntityEncoder, InvalidMessageBodyFailure}
import org.http4s.rho.RhoRoutes
import org.http4s.rho.swagger.SwaggerSyntax

import scala.language.higherKinds

object WebApiRoutes {
  def service[F[+_]: Effect: Log, M[_]: Sync](
      webApi: WebApi[M],
      swaggerSyntax: SwaggerSyntax[F]
  )(implicit mf: M ~> F): RhoRoutes[F] =
    new RhoRoutes[F] {
      import coop.rchain.casper.protocol.{BlockInfo, LightBlockInfo}
      import io.circe._
      import io.circe.generic.auto._
      import org.http4s.circe._
      import org.http4s.rho.bits.StringParser
      import swaggerSyntax._

      // TODO: Create generic encoders/decoders for
      // ADT's with discriminator field
      implicit val encodeByteString: Encoder[ByteString] =
        Encoder.encodeString.contramap[ByteString](PrettyPrinter.buildStringNoLimit)
      implicit val encodeLightBlockInfo: Encoder[LightBlockInfo] = deriveEncoder[LightBlockInfo]

      implicit val encodeBlockInfo: Encoder[BlockInfo] = deriveEncoder[BlockInfo]

      // Encoders
      implicit val booleanEncode: EntityEncoder[F, Boolean]      = jsonEncoderOf[F, Boolean]
      implicit val stringEncode: EntityEncoder[F, String]        = jsonEncoderOf[F, String]
      implicit val apiStatusEncoder: EntityEncoder[F, ApiStatus] = jsonEncoderOf[F, ApiStatus]
      implicit val blockInfoEncoder: EntityEncoder[F, BlockInfo] = jsonEncoderOf[F, BlockInfo]
      implicit val lightBlockEncoder: EntityEncoder[F, LightBlockInfo] =
        jsonEncoderOf[F, LightBlockInfo]
      implicit val lightBlockListEnc: EntityEncoder[F, List[LightBlockInfo]] =
        jsonEncoderOf[F, List[LightBlockInfo]]
      implicit val dataRespEncoder: EntityEncoder[F, DataResponse] = jsonEncoderOf[F, DataResponse]
      implicit val prepareEncoder: EntityEncoder[F, PrepareResponse] =
        jsonEncoderOf[F, PrepareResponse]
      implicit val explRespEncoder: EntityEncoder[F, ExploratoryDeployResponse] =
        jsonEncoderOf[F, ExploratoryDeployResponse]
      // Decoders
      implicit val deployRequestDecoder: EntityDecoder[F, DeployRequest] = jsonOf[F, DeployRequest]
      implicit val dataRequestDecoder: EntityDecoder[F, DataRequest]     = jsonOf[F, DataRequest]
      implicit val prepareDecoder: EntityDecoder[F, PrepareRequest]      = jsonOf[F, PrepareRequest]
      implicit val exploreDeployRequestDecoder: EntityDecoder[F, ExploreDeployRequest] =
        jsonOf[F, ExploreDeployRequest]
      implicit val stringDecoder: EntityDecoder[F, String] = jsonOf[F, String]
      // uri path parser
      implicit val stringParser = StringParser.strParser[F]
      implicit val intParser    = StringParser.intParser[F]

      def getErrorMsg(ex: Throwable): String =
        if (ex.getMessage == null) ex.toString else ex.getMessage
      def handleError(err: Throwable) = err match {
        // The place where all API errors are handled
        // TODO: introduce error codes
        case err: InvalidMessageBodyFailure =>
          // Request JSON parse errors
          s"${getErrorMsg(err).take(250)}..."
        // Errors from BlockAPI
        case err: BlockApiException =>
          getErrorMsg(err)
        case err: Throwable =>
          // Logging only unanticipated errors, not related to Block API or input parsing (user errors)
          getErrorMsg(err)
      }

      implicit class MEx[A](val ma: M[A]) {
        def handle(implicit e: EntityEncoder[F, A]) =
          mf(ma).attemptT
            .fold(
              e => BadRequest(handleError(e)),
              Ok(_)
            )
            .flatten
      }

      "Get the version of the api and message" **
        "others" @@ GET / "status" |>> { () =>
        webApi.status.handle
      }

      // Prepare deploy
      "Get the prepare deploy info" **
        "rhovm" @@ GET / "prepare-deploy" |>> { () =>
        webApi
          .prepareDeploy(none)
          .handle
      }

      ("Get ids of new names to be allocated in an upcoming deploy; " +
        "for example, in case the term needs to include a signature over them.") **
        "rhovm" @@ POST / "prepare-deploy" ^ prepareDecoder |>> { (req: PrepareRequest) =>
        webApi
          .prepareDeploy(req.some)
          .handle
      }

      // Deploy
      "Request execution of a rholang term on the RhoVM." **
        "rhovm" @@ POST / "deploy" ^ deployRequestDecoder |>> { (req: DeployRequest) =>
        webApi
          .deploy(request = req)
          .handle
      }

      ("Run rholang term in an exploratory mode where the results are not persisted in the consensus state." +
        " To get a result, send to the first new name. For example: `new return in { return!(1+1) }`.") **
        "rhovm" @@ POST / "explore-deploy" ^ stringDecoder |>> { term: String =>
        webApi
          .exploratoryDeploy(term, none[String], usePreStateHash = false)
          .handle
      }

      "Run an exploratory deploy as of a specific block's state." **
        "rhovm" @@ POST / "explore-deploy-by-block-hash" ^ exploreDeployRequestDecoder |>> {
        req: ExploreDeployRequest =>
          if (req.blockHash.isEmpty)
            webApi
              .exploratoryDeploy(req.term, none[String], req.usePreStateHash)
              .handle
          else
            webApi
              .exploratoryDeploy(req.term, Some(req.blockHash), req.usePreStateHash)
              .handle
      }

      // Get data
      "Get the data from specific name channel" **
        "rhovm" @@ POST / "data-at-name" ^ dataRequestDecoder |>> { (req: DataRequest) =>
        webApi
          .listenForDataAtName(req)
          .handle
      }

      //Blocks
      "Get the last finalized block from the node. The result is based on what the nodes have" **
        "blocks" @@ GET / "last-finalized-block" |>> { () =>
        webApi.lastFinalizedBlock.handle
      }

      "Get the specific block info from the block hash" **
        "blocks" @@ GET / "block" / pathVar[String]("blockHash") |>> { (hash: String) =>
        webApi
          .getBlock(hash)
          .handle
      }

      "Get block hash etc. for the most recent block seen by this node" **
        "blocks" @@ GET / "blocks" |>> webApi
        .getBlocks(1)
        .handle

      "Get blocks from the range between startBlockNumber and endBlockNumber. The max range is limited by the node configuration." **
        "blocks" @@ GET / "blocks" / pathVar[Int]("startBlockNumber") / pathVar[Int](
        "endBlockNumber"
      ) |>> { (startBlockNumber: Int, endBlockNumber: Int) =>
        webApi
          .getBlocksByHeights(startBlockNumber.toLong, endBlockNumber.toLong)
          .handle
      }

      "Get the latest n blocks from the node" **
        "blocks" @@ GET / "blocks" / pathVar[Int]("depth") |>> { (depth: Int) =>
        webApi
          .getBlocks(depth)
          .handle
      }

      "Get the lightBlockInfo which contains the specified deployId" **
        "blocks" @@ GET / "deploy" / pathVar[String]("deployId") |>> { (deployId: String) =>
        webApi
          .findDeploy(deployId)
          .handle
      }

      "Get whether the block is finalized or not." **
        "blocks" @@ GET / "is-finalized" / pathVar[String]("blockHash") |>> { hash: String =>
        webApi
          .isFinalized(hash)
          .handle
      }
    }

}
