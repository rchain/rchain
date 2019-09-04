package coop.rchain.node

import cats.effect.Sync
import coop.rchain.node.effects.EventConsumer
import fs2.Pipe
import org.http4s.HttpRoutes
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame._

import scala.{Stream => _}

object EventsInfo {
  def service[F[_]: EventConsumer: Sync]: HttpRoutes[F] = {
    import io.circe.generic.auto._
    import io.circe.syntax._
    import org.http4s.circe.CirceEntityEncoder._

    val dsl = org.http4s.dsl.Http4sDsl[F]
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root =>
        val data                                = EventConsumer[F].consume.map(rca => rca.asJson).map(j => Text(j.noSpaces))
        val noop: Pipe[F, WebSocketFrame, Unit] = _.evalMap(_ => Sync[F].unit)
        WebSocketBuilder[F].build(data, noop)
    }
  }
}
