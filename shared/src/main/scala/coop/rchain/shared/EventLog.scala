package coop.rchain.shared

import cats.~>

sealed trait Event
object Event {
  final case class NodeStarted(address: String)                             extends Event
  final case class EnteredRunningState(blockHash: String)                   extends Event
  final case class ApprovedBlockReceived(blockHash: String)                 extends Event
  final case class SentUnapprovedBlock(blockHash: String)                   extends Event
  final case class BlockApprovalReceived(blockHash: String, sender: String) extends Event
  final case class SentApprovedBlock(blockHash: String)                     extends Event
}

trait EventLog[F[_]] {
  def publish(event: Event): F[Unit]
}

object EventLog {
  def apply[F[_]](implicit E: EventLog[F]): EventLog[F] = E

  implicit val EventLoggerLogSource: LogSource = LogSource(classOf[EventLogger])

  def eventLogger[F[_]: Log]: EventLog[F] =
    (event: Event) => Log[F].info(event.getClass.getSimpleName.stripSuffix("$"))

  // FunctorK
  implicit class EventLogMapKOps[F[_]](val el: EventLog[F]) extends AnyVal {
    def mapK[G[_]](nt: F ~> G): EventLog[G] = new EventLog[G] {
      override def publish(event: Event): G[Unit] = nt(el.publish(event))
    }
  }
}

class EventLogger() // Dummy class for the logger name
