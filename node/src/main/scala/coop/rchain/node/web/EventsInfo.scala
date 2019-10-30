package coop.rchain.node.web

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import coop.rchain.node.effects.EventConsumer
import coop.rchain.shared.RChainEvent
import fs2.Pipe
import fs2.concurrent.Topic
import io.circe.Json
import org.http4s.HttpRoutes
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text

object EventsInfo {

  /**
    * The goal of this service is to provide a live view into what is happening in the node without grokking logs.
    *
    * This service is designed to drain all the data from an `EventConsumer` source
    * and push them to all WebSocket clients
    *
    * The source defines a `def consume` method which results in a stream connected to an internal Queue.
    * This could be connected straight to `WebSocketBuilder`
    * but would result in a round-robin type behaviour for client_size > 1.
    *
    * To overcome this behavior an internal topic is introduced that pushes ALL the events from the mentioned Queue
    * to the internal topic.
    *
    * The topic is able to replicate the messages to all its current clients (broadcast).
    *
    * The topic itself caches only 10 messages and the queue should do no caching at all as it will just be drained.
    */
  def service[F[_]: EventConsumer: Sync: Concurrent]: F[HttpRoutes[F]] = {

    import io.circe.generic.extras.Configuration
    import io.circe.generic.extras.auto._
    import io.circe.syntax._

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
      dataTopic = topic.subscribe(maxQueued = 10)
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
