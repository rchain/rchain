package coop.rchain.node.service

import cats.effect._
import cats.data.EitherT
import org.http4s._
import org.http4s.client._
import org.http4s.circe._
import org.http4s.client.dsl.io._
import org.http4s.{Request => HttpRequest}
import org.http4s.dsl.io._
import io.circe._
import io.circe.parser._
import io.circe.literal._
import io.circe.syntax._
import io.circe.generic.auto._
import io.circe.generic.semiauto._

import io.circe.generic.JsonCodec

import shapeless._

object JsonRpc {
  type Primitive = Double :+: Long :+: String :+: Boolean :+: CNil
  type IdType    = Long :+: String :+: CNil

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
        v.as[Long] match {
          case Right(i) => Right(Coproduct[IdType](i))
          case _ =>
            Left(DecodingFailure("id must be Number or String", c.history))
        }
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
  case class Response(jsonrpc: String, id: IdType, result: JsonResult) extends JsonResponse
  case class Error(code: Int, message: String, data: ErrorData)        extends JsonResponse

  sealed trait JsonResult
  case class NodeVersionResult(nodeVersion: String, apiVersion: String) extends JsonResult
  case class NodeStatusResult(status: String, timestamp: String)        extends JsonResult

  sealed trait ErrorData
  case class SomethingBroke(what: String) extends ErrorData

  val service = HttpService[IO] {
    case req @ POST -> Root => {
      println(s"   req: $req")

      val request =
        for {
          request <- req.as[Request]
        } yield request

      val foo = request.unsafeRunSync

      Ok(s"""{"foo": "${foo}"}""")
    }
  }
}
