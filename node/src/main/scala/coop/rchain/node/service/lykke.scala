package coop.rchain.node.service

import cats.effect._
import org.http4s._
import org.http4s.dsl.io._

object Lykke {
  val service = HttpService[IO] {
    case GET -> Root         => Ok(s"Roger that: Lykke at <root>.\n")
    case GET -> Root / "get" => Ok(s"Roger that: Lykke at <root> / get.\n")
  }
}
