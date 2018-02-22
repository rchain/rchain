package coop.rchain.node.service

import cats.effect._
import cats.data.EitherT
import org.http4s._
import org.http4s.dsl.io._
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.auto._
import io.circe.generic.semiauto._

import io.circe.generic.JsonCodec

import shapeless._

object JsonRpc {
  type Primitive = Double :+: Int :+: String :+: Boolean :+: CNil
  type IdType    = Int :+: String :+: CNil

  implicit val encodeIdType: Encoder[IdType] = new Encoder[IdType] {
    final def apply(x: IdType): Json =
      x match {
        case Inl(i)      => Json.fromInt(i)
        case Inr(Inl(s)) => Json.fromString(s)
        case _           => ???
      }
  }

  implicit val decodeIdType: Decoder[IdType] = new Decoder[IdType] {
    final def apply(c: HCursor): Decoder.Result[IdType] =
      Right(Coproduct[IdType](300))
  }

  sealed trait JsonRequest
  case class Request(jsonrpc: String, method: String, id: IdType) extends JsonRequest

  // implicit val requestDecoder: EntityDecoder[IO, Request] =
  //   EntityDecoder.decodeBy(MediaRange.`*/*`) { msg =>
  //     EitherT {
  //       msg.as[String].map(s => Json.fromString(s).as[Request]).map(_.asRight[DecodeFailure])
  //     }
  //   }

  // implicit val requestDecoder: EntityDecoder[IO, Request] = new EntityDecoder[IO, Request] {
  //   override def consumes: Set[MediaRange]                                            = ???
  //   override def decode(msg: Message[IO], strict: Boolean): DecodeResult[IO, Request] =
  //     // cats.data.EitherT.right(msg.as[Request])
  //     cats.data.EitherT.right(IO { x: Message[IO] =>
  //       msg.unsafeRunSync match {
  //         case s: String => s.as[Request]
  //         case _         => ???
  //       }
  //     })
  // }
  // implicit val requestEncoder: Decoder[Byte]              = new Decoder[Byte]

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
      // val foo = req.as[Request] //.asJson.as[Request]
      val foo = req.as(jsonOf[Request])
      // for {
      //   b <- req.as(jsonOf[Request])
      //   j = Json.fromString(b.as[String])
      //   r <- j.as[Request]
      // } yield r
      // val foo = Json.fromString(req.flatMap(_.as[String])).as[Request] //.asJson.as[Request]
      // for {
      //   ix <- req.as[Request]
      //   seq     = ix.id
      //   method  = ix.method
      //   version = ix.jsonrpc
      //   request = Request(version, method, seq)
      // } yield request

      foo.unsafeRunSync match {
        case request: Request =>
          request.jsonrpc match {
            case "2.0" =>
              println("Got right version")
            case x =>
              println(s"Got version ‘${x}’")
          }
      }

      println(s"   foo: ${foo}")

      // match {
      //   case Right(r) => {
      //     println("right: " + r)
      //     val resp = Response("2.0", Coproduct[IdType](33), NodeVersionResult("1.1", "2.2"))
      //     Ok(s"${resp.asJson}")
      //   }
      //   case Left(ugh) => {
      //     println("left: " + ugh)
      //     Ok(s"""${Error(12, "foo", SomethingBroke(ugh.toString)).asJson}""")
      //   }
      // }
      Ok("")

      // val js  = """{"jsonrpc": "2.0", "method": "foozle", "id": 31}"""
      // val jsj = js.asJson
      // println(s"json: $jsj")
      // val rrr = req.as(jsonOf[Request])
      // println(s"rrr: $rrr")
      // val body = req.body
      // println(s"body: $body")
      // decode[Request](body) match {
      // val foo = for {
      //   s <- body.map(_.toString)
      //   x = Json.fromString(s)
      //   q <- x.as[Request]
      // } yield q
      // // req.as[Request] // match  {
      // //   case Right(r) => {
      // //     println("right: " + r)
      // //     val resp = Response("2.0", Coproduct[IdType](33), NodeVersionResult("1.1", "2.2"))
      // //     Ok(s"${resp.asJson}")
      // //   }
      // //   case Left(ugh) => {
      // //     println("left: " + ugh)
      // //     Ok(s"""${Error(12, "foo", SomethingBroke(ugh.toString)).asJson}""")
      // //   }
      // // }

      // println(s"foo: $foo")
      // Ok("FOO")

      // for {
      //   r <- js.asJson.as[Request]
      // } yield Ok(s"${resp.asJson}")

      // // .as(jsonOf[jsonrpc.Request])
      // // .flatMap((request: Request) =>
      // //   Ok(handleRequestWrapper(handleRequest, request).map(_.asJson)))

      // val res = Response("2.0", Coproduct[IdType](33), NodeVersionResult("1.1", "2.2"))
      // Ok(s"${res.asJson}")
    }
  }
}
