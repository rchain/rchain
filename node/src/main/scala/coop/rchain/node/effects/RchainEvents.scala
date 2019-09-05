package coop.rchain.node.effects

import cats.data.ReaderT
import cats.effect.{Concurrent, Sync}
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
  def readerTInstance[F[_]: Sync: Concurrent, E]: ReaderT[F, E, RchainEvents[ReaderT[F, E, ?], F]] =
    for {
      q <- Queue.in[ReaderT[F, E, ?]].circularBuffer[F, RChainEvent](1)
    } yield new RchainEvents[ReaderT[F, E, ?], F] {
      override def publish(e: => RChainEvent): ReaderT[F, E, Unit] = ReaderT.liftF(
        q.enqueue1(e)
      )

      override def consume: FS2Stream[F, RChainEvent] =
        q.dequeue
    }

}
