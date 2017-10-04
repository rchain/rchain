package coop.rchain.comm

import io.circe._
import io.circe.generic._
import io.circe.syntax._

import org.http4s._
import org.http4s.dsl._
import org.http4s.circe._
import org.http4s.server._
import org.http4s.server.blaze._

import coop.rchain.kv._

@JsonCodec case class KVPair(key: String, value: String)

class HttpServer(port: Int, msgHandler: MessageHandler) {

  var server: Server = null

  val service = HttpService {
    case GET -> Root =>
      Ok("Roger that.\n")
    case GET -> Root / "set" / key / value =>
      msgHandler.sendMutation(key, value)
      Ok(s"Setting $key to $value")
    case req @ POST -> Root / "set" =>
      for {
        r <- req.as(jsonOf[KVPair])
        resp <- {
          msgHandler.sendMutation(r.key, r.value)
          Ok(s"Setting ${r key} to ${r value}")
        }
      } yield (resp)
    case GET -> Root / "get" / key =>
      val queryOutcome = msgHandler.query(key)
      Ok(((queryOutcome.standard asJson) noSpaces) + "\n")
    case GET -> Root / "get" =>
      // Ok("Fetch what?!")
      Ok(msgHandler getAll)
    case GET -> Root / "dump" =>
      Ok(msgHandler dump)

    case GET -> Root / "peers" =>
      Ok(msgHandler peers)
  }

  val bld = BlazeBuilder
    .bindHttp(port, "localhost")
    .mountService(service, "/")

  def start = {
    println(s"Starting HTTP on $port.")
    server = bld run
  }

  def stop =
    if (server != null) server shutdownNow
}
