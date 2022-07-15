package coop.rchain.casper.helper
import cats.effect.Concurrent
import coop.rchain.metrics.Span
import coop.rchain.models.ListParWithRandom
import coop.rchain.models.rholang.RhoType
import coop.rchain.rholang.interpreter.SystemProcesses.ProcessContext
import coop.rchain.rholang.interpreter.{ContractCall, PrettyPrinter}
import coop.rchain.shared.{Log, LogSource}

object RhoLoggerContract {
  val prettyPrinter = PrettyPrinter()

  //TODO extract a `RhoPatterns[F]` algebra that will move passing the Span, the Dispatcher, and the Space parameters closer to the edge of the world
  def handleMessage[F[_]: Log: Concurrent: Span](
      ctx: ProcessContext[F]
  )(message: Seq[ListParWithRandom]): F[Unit] = {
    val isContractCall = new ContractCall(ctx.space, ctx.dispatcher)

    message match {
      case isContractCall(_, Seq(RhoType.String(logLevel), par)) =>
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
}
