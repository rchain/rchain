package coop.rchain.node

import cats.effect._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.server._
import org.http4s.server.blaze._
import com.typesafe.scalalogging.Logger

// @JsonCodec case class KVPair(key: String, value: String)

case class HttpServer(port: Int) {

  val logger = Logger("main")

  var server: Option[Server[IO]] = None

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

  val lykke = HttpService[IO] {
    case GET -> Root         => Ok(s"Roger that: Lykke at <root>.\n")
    case GET -> Root / "get" => Ok(s"Roger that: Lykke at <root> / get.\n")
  }

  val bld = BlazeBuilder[IO]
    .bindHttp(port, "localhost")
    .mountService(service, "/")
    .mountService(lykke, "/lykke")
    .start

  def start(): Unit = {
    logger.info(s"HTTP server started on port $port.")
    server = Some(bld.unsafeRunSync())
  }

  def stop(): Unit =
    server.foreach(_.shutdown.unsafeRunSync())
  logger.info("HTTP server stopped.")
}
