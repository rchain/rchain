package coop.rchain.node

import cats.implicits._
import cats.effect.{Concurrent, ExitCode, Sync}
import coop.rchain.node.effects.EventConsumer
import coop.rchain.shared.RChainEvent
import fs2.Pipe
import fs2.concurrent.Topic
import io.circe.Json
import org.http4s.HttpRoutes
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame._

import scala.{Stream => _}

object EventsInfo {
  def service[F[_]: EventConsumer: Sync: Concurrent]: F[HttpRoutes[F]] = {

    import io.circe.syntax._
    import io.circe.generic.extras.auto._
    import io.circe.generic.extras.Configuration

    val eventTypeLabel     = "event"
    val schemaVersionLabel = "schema-version"
    val schemaVersionJson  = Json.fromInt(1)

    val startedEvent = Text(
      Json
        .obj(
          ("event", Json.fromString("started")),
          (schemaVersionLabel, schemaVersionJson)
        )
        .noSpaces
    )

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

    for {
      topic <- Topic[F, WebSocketFrame](startedEvent)
      consumer = EventConsumer[F].consume
        .map(transformRChainEvent)
        .map(j => Text(j.noSpaces))
        .flatMap(fs2.Stream.emit)
        .through(topic.publish)
      dataTopic = topic.subscribe(10)
      _         <- Concurrent[F].start(consumer.compile.drain)
      routes = {
        val dsl = org.http4s.dsl.Http4sDsl[F]
        import dsl._

        HttpRoutes.of[F] {
          case GET -> Root =>
            val noop: Pipe[F, WebSocketFrame, Unit] = _.evalMap(_ => Sync[F].unit)
            WebSocketBuilder[F].build(dataTopic, noop)
        }
      }
    } yield routes
  }
}
