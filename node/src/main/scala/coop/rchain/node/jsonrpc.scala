/**
package coop.rchain.node.service

// import io.circe._
import io.circe.generic.auto._
// import io.circe.generic.JsonCodec
// import io.circe.parser._
import io.circe.syntax._

sealed trait JsonRPCResponse
case class Response[T](jsonrpc: String, id: T) extends JsonRPCResponse

// val foo: Foo = Qux(13, Some(14.0))

// val json = foo.asJson.noSpaces
// println(json)

// val decodedFoo = decode[Foo](json)
// println(decodedFoo)

object JsonRpc {
  val resp0 = Response("2.0", "foo")
  val resp1 = Response("2.0", 33)
  override def toString(): String =
    resp0.asJson.noSpaces + " -- " + resp1.asJson.noSpaces
  // List[JsonRPCResponse](resp0, resp1).asJson.noSpaces
}
  */
