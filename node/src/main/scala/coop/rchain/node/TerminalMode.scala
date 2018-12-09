package coop.rchain.node

trait TerminalMode[F[_]] {
  def interactive: Option[Unit]
}

object TerminalMode {
  def apply[F[_]](implicit ev: TerminalMode[F]): TerminalMode[F] = ev
}

object TerminalModeInstances {
  implicit def consoleBasedTTY[F[_]]: TerminalMode[F] = new TerminalMode[F] {
    val value = if (System.console() != null) Some(()) else None
    override def interactive: Option[Unit] = value
  }
}