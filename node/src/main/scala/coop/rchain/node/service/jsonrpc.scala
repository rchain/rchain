package coop.rchain.node.service

import cats.effect._
import io.circe._
import io.circe.generic.auto._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras._
import io.circe.literal._
import io.circe.syntax._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._

import shapeless._

object jsonrpc {
  implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames

  type IdType = Long :+: String :+: CNil

  implicit val encodeIdType: Encoder[IdType] = new Encoder[IdType] {
    final def apply(x: IdType): Json =
      x match {
        case Inl(i)      => Json.fromLong(i)
        case Inr(Inl(s)) => Json.fromString(s)
        case _           => ???
      }
  }

  implicit val decodeIdType: Decoder[IdType] = new Decoder[IdType] {
    final def apply(c: HCursor): Decoder.Result[IdType] = {
      val v = c.value
      if (v.isNumber) {
        v.as[Long].flatMap(i => Right(Coproduct[IdType](i)))
      } else if (v.isString) {
        Right(Coproduct[IdType](v.asString.getOrElse("")))
      } else {
        Left(DecodingFailure("id must be Number or String", c.history))
      }
    }
  }

  sealed trait JsonRequest
  case class Request(jsonrpc: String, method: String, id: IdType) extends JsonRequest

  implicit val requestEncoder = jsonOf[IO, Request]

  sealed trait JsonResponse
  @ConfiguredJsonCodec case class Response(jsonrpc: String, id: IdType, result: JsonResult)
      extends JsonResponse
  @ConfiguredJsonCodec case class Error(jsonrpc: String, id: IdType, error: ErrorData)
      extends JsonResponse

  object JsonResponse {
    implicit val encodeJsonResponse: Encoder[JsonResponse] = Encoder.instance {
      case response @ Response(_, _, _) => response.asJson
      case error @ Error(_, _, _)       => error.asJson
    }
  }

  sealed trait JsonResult
  @ConfiguredJsonCodec case class NodeVersionResult(nodeVersion: String, apiVersion: String)
      extends JsonResult
  @ConfiguredJsonCodec case class NodeStatusResult(status: String, timestamp: String)
      extends JsonResult

  object JsonResult {
    implicit val encodeJsonResult: Encoder[JsonResult] = Encoder.instance {
      case result @ NodeVersionResult(_, _) => result.asJson
      case result @ NodeStatusResult(_, _)  => result.asJson
    }
  }

  sealed trait ErrorData
  @ConfiguredJsonCodec case class StandardError(code: Int, message: String) extends ErrorData
  @ConfiguredJsonCodec case class ExtendedError(code: Int, message: String, data: ErrorDetail)
      extends ErrorData

  sealed trait ErrorDetail
  @ConfiguredJsonCodec case class SomethingBroke(what: String) extends ErrorDetail

  object ErrorData {
    implicit val encodeErrorData: Encoder[ErrorData] = Encoder.instance {
      case standard @ StandardError(_, _)    => standard.asJson
      case extended @ ExtendedError(_, _, _) => extended.asJson
    }

    val parseError          = StandardError(-32700, "Parse error")
    val invalidRequestError = StandardError(-32600, "Invalid Request")
    val methodNotFoundError = StandardError(-32601, "Method not found")
    val invalidParamsError  = StandardError(-32602, "Invalid params")
    val internalError       = StandardError(-32603, "Internal error")
  }

  object ErrorDetail {
    implicit val encodeErrorDetail: Encoder[ErrorDetail] = Encoder.instance {
      case broke @ SomethingBroke(_) => broke.asJson
    }

  }

  def handle(req: Request): JsonResponse =
    req.jsonrpc match {
      case "2.0" =>
        req.method match {
          case "node_version" =>
            Response("2.0", req.id, NodeVersionResult("0.1", "0.2"))
          case "node_status" =>
            Response("2.0", req.id, NodeStatusResult("up", "<timestamp>"))
          case _ =>
            Error("2.0", req.id, ErrorData.methodNotFoundError)
        }
      case _ => Error("2.0", req.id, ErrorData.invalidRequestError)
    }

  val service = HttpService[IO] {
    case req @ POST -> Root => {
      val response =
        for {
          request <- req.as[Request]
          resp    <- IO { handle(request) }
          result  <- IO { resp.asJson }
        } yield result

      Ok(response.unsafeRunSync)
    }
  }
}
