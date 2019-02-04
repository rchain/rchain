package coop.rchain.casper.helper
import coop.rchain.models.{ListParWithRandomAndPhlos, Par}
import coop.rchain.rholang.interpreter.PrettyPrinter
import coop.rchain.rholang.interpreter.Runtime.SystemProcess
import coop.rchain.shared.{Log, LogSource}

object RhoLogger {
  val prettyPrinter = PrettyPrinter()

  object IsLogMessage {
    def unapply(p: Seq[ListParWithRandomAndPhlos]): Option[(String, Par)] =
      p match {
        case Seq(
            ListParWithRandomAndPhlos(
              Seq(IsString(logLevel), par),
              _,
              _
            )
            ) =>
          Some((logLevel, par))
        case _ => None
      }
  }

  def handleMessage[F[_]: Log](
      ctx: SystemProcess.Context[F]
  )(message: Seq[ListParWithRandomAndPhlos], x: Int): F[Unit] =
    message match {
      case IsLogMessage(logLevel, par) =>
        val msg         = prettyPrinter.buildString(par)
        implicit val ev = LogSource.matLogSource

        logLevel match {
          case "trace" => Log[F].trace(msg)
          case "debug" => Log[F].debug(msg)
          case "info"  => Log[F].info(msg)
          case "warn"  => Log[F].warn(msg)
          case "error" => Log[F].error(msg)
        }
    }
}
