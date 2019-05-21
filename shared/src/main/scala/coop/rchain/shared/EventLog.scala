package coop.rchain.shared

sealed trait Event
object Event {
  object NodeStarted           extends Event
  object EnteredRunningState   extends Event
  object ApprovedBlockReceived extends Event
  object SentUnapprovedBlock   extends Event
  object BlockApprovalReceived extends Event
  object SentApprovedBlock     extends Event
}

trait EventLog[F[_]] {
  def publish(event: Event): F[Unit]
}

object EventLog {
  def apply[F[_]](implicit E: EventLog[F]): EventLog[F] = E

  implicit val EventLoggerLogSource: LogSource = LogSource(classOf[EventLogger])

  def eventLogger[F[_]: Log]: EventLog[F] =
    (event: Event) => Log[F].info(event.getClass.getSimpleName.stripSuffix("$"))
}

class EventLogger() // Dummy class for the logger name
