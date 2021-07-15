package coop.rchain.node.proposescript

import cats.effect.ExitCode
import coop.rchain.shared.Log
import fs2.Stream
import monix.eval.{Task, TaskApp}
import org.rogach.scallop.ScallopConf
import pureconfig.ConfigSource
import pureconfig.generic.auto._
import java.nio.file.Path

final case class ProposeScriptOptions(arguments: Seq[String]) extends ScallopConf(arguments) {
  val width = 120
  helpWidth(width)
  printedName = "propose-script"

  val config = opt[Path](
    descr = s"Target config file path",
    required = true
  )
  verify()

}
object ProposeMain extends TaskApp {
  override def run(args: List[String]): Task[ExitCode] = {
    val options      = ProposeScriptOptions(args)
    val configPath   = options.config()
    val configSource = ConfigSource.file(configPath)
    val configE      = configSource.load[ProposeConfig]
    val config       = configE.right.get
    implicit val log = Log.log[Task]

    for {
      _ <- log.info(s"Running propose script with config: ${config}")
      _ <- Stream
            .eval(
              config.proposeRound.tapError(
                e => log.error(s"Running propose script with config: ${config} with error ${e}")
              )
            )
            .repeat
            .compile
            .drain
    } yield ExitCode.Success
  }
}
