package coop.rchain.shared

import cats.Applicative

sealed trait RChainEvent {}

final case class BlockCreated(hash: String)   extends RChainEvent
final case class BlockFinalised(hash: String) extends RChainEvent

object RChainEvent {
  def created(bs: => String): RChainEvent = BlockCreated(bs)
}

trait EventPublisher[F[_]] {
  def publish(e: => RChainEvent): F[Unit]
}
object EventPublisher {
  def apply[F[_]](implicit P: EventPublisher[F]): EventPublisher[F] = P

  def noop[F[_]: Applicative] = new EventPublisher[F] {
    override def publish(e: => RChainEvent): F[Unit] = Applicative[F].unit
  }
}
