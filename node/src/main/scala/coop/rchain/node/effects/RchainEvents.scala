package coop.rchain.node.effects

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.shared.{EventPublisher, RChainEvent}
import fs2.concurrent.Queue
import fs2.{Stream => FS2Stream}

trait EventConsumer[F[_]] {
  def consume: FS2Stream[F, RChainEvent]
}

object EventConsumer {
  def apply[F[_]](implicit C: EventConsumer[F]): EventConsumer[F] = C
}

trait RchainEvents[F[_], G[_]] extends EventPublisher[F] with EventConsumer[G]

object RchainEvents {

  def apply[F[_]: Sync: Concurrent]: F[RchainEvents[F, F]] =
    for {
      q <- Queue.in[F].circularBuffer[F, RChainEvent](1)
    } yield new RchainEvents[F, F] {
      override def publish(e: => RChainEvent): F[Unit] = q.enqueue1(e)

      override def consume: FS2Stream[F, RChainEvent] = q.dequeue
    }

}
