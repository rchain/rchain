package coop.rchain.node.service

import cats.effect._
import org.http4s._
import org.http4s.dsl.io._
import io.circe.generic.auto._
import io.circe.syntax._

sealed trait JsonRPCResponse
case class Response[T](jsonrpc: String, id: T) extends JsonRPCResponse

object JsonRpc {
  val resp0 = Response("2.0", "foo")
  val resp1 = Response("2.0", 33)
  override def toString(): String =
    resp0.asJson.noSpaces + " -- " + resp1.asJson.noSpaces
}

object JsonRPC {
  val service = HttpService[IO] {
    case POST -> Root =>
      Ok(s"POST call at <root> => ${JsonRpc}.\n")
    case GET -> Root =>
      Ok("GET call at <root>.\n")
  }

}
