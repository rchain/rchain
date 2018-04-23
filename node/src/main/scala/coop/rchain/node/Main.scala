package coop.rchain.node

import cats._, cats.data._, cats.implicits._
import scala.tools.jline.console._, completer.StringsCompleter
import scala.collection.JavaConverters._

import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib._
import monix.eval.Task

object Main {

  def main(args: Array[String]): Unit = {
    val conf    = Conf(args)
    val console = new ConsoleReader()
    console.setHistoryEnabled(true)
    console.setPrompt("rholang $")
    console.addCompleter(new StringsCompleter(ReplRuntime.keywords.asJava))

    import monix.execution.Scheduler.Implicits.global

    implicit val consoleIO: ConsoleIO[Task] = new effects.JLineConsoleIO(console)
    implicit val replService: ReplService[Task] =
      new GrpcReplService(conf.grpcHost(), conf.grpcPort())

    (conf.eval.toOption, conf.repl()) match {
      case (Some(fileName), _) => new ReplRuntime(conf).evalProgram[Task](fileName)
      case (None, true)        => new ReplRuntime(conf).replProgram[Task]
      case (None, false) =>
        new NodeRuntime(conf).nodeProgram.value.map {
          case Right(_) => ()
          case Left(commError) =>
            throw new Exception(commError.toString) // TODO use Show instance instead
        }
    }
  }

}
