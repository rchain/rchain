package coop.rchain.node.service

import cats.effect._
import org.http4s._
import org.http4s.dsl.io._
import io.circe.generic.auto._
import io.circe.syntax._

// import io.circe._
// import io.circe.generic.JsonCodec
// import io.circe.parser._

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

object JsonRPC {
  val service = HttpService[IO] {
    case POST -> Root =>
      Ok(s"JSON-RPC call at <root> => ${JsonRpc.toString}.\n")

    case GET -> Root =>
      Ok("Roger that.\n")
    case GET -> Root / "set" / key / value =>
      Ok(s"Roger that; key = $key; value = $value.\n")
    case req @ POST -> Root / "set" =>
      Ok(s"Roger that; req = $req.\n")
    // for {
    //   r <- req.as(jsonOf[KVPair])
    //   resp <- {
    //     msgHandler.sendMutation(r.key, r.value)
    //     Ok(s"Setting ${r key} to ${r value}")
    //   }
    // } yield (resp)
    case GET -> Root / "get" / key =>
      Ok(s"Roger that; key = $key.\n")
    // val queryOutcome = msgHandler.query(key)
    // Ok(((queryOutcome.standard asJson) noSpaces) + "\n")
    case GET -> Root / "get" =>
      Ok("Fetch what?!")
    // Ok(msgHandler getAll)
    case GET -> Root / "dump" =>
      Ok("Roger that.\n")
    // Ok(msgHandler dump)

    case GET -> Root / "peers" =>
      Ok("Roger that.\n")
    // Ok(msgHandler peers)
  }

}
