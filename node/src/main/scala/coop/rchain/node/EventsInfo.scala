package coop.rchain.node

import cats.effect.Sync
import coop.rchain.node.effects.EventConsumer
import coop.rchain.shared.RChainEvent
import fs2.Pipe
import io.circe.Json
import org.http4s.HttpRoutes
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame._

import scala.{Stream => _}

object EventsInfo {
  def service[F[_]: EventConsumer: Sync]: HttpRoutes[F] = {

    import io.circe.syntax._
    import io.circe.generic.extras.auto._
    import io.circe.generic.extras.Configuration

    val eventTypeLabel     = "event"
    val schemaVersionLabel = "schema-version"
    val schemaVersionJson  = Json.fromInt(1)

    implicit val genDevConfig: Configuration =
      Configuration.default
        .withDiscriminator(eventTypeLabel)
        .withKebabCaseConstructorNames
        .withKebabCaseMemberNames

    def transformRChainEvent(e: RChainEvent): Json = {
      val serialized = e.asJson
      val eventType  = serialized.findAllByKey(eventTypeLabel).head
      Json.obj(
        ("event", eventType),
        (schemaVersionLabel, schemaVersionJson),
        ("payload", serialized.mapObject(_.remove(eventTypeLabel)))
      )
    }

    val dsl = org.http4s.dsl.Http4sDsl[F]
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root =>
        val data =
          EventConsumer[F].consume.map(rca => transformRChainEvent(rca)).map(j => Text(j.noSpaces))
        val noop: Pipe[F, WebSocketFrame, Unit] = _.evalMap(_ => Sync[F].unit)
        WebSocketBuilder[F].build(data, noop)
    }
  }
}
