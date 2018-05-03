package coop.rchain.node

import coop.rchain.shared.StringOps._
import cats._, cats.data._, cats.implicits._
import scala.tools.jline.console._, completer.StringsCompleter
import scala.collection.JavaConverters._

import coop.rchain.comm._
import coop.rchain.casper.util.comm.{DeployRuntime, DeployService, GrpcDeployService}
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib._
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService
import scala.concurrent._
import scala.concurrent.duration._

object Main {

  def main(args: Array[String]): Unit = {
    val conf = Conf(args)

    implicit val io: SchedulerService = Scheduler.io("repl-io")

    implicit val replService: ReplService[Task] =
      new GrpcReplService(conf.grpcHost(), conf.grpcPort())
    implicit val diagnosticsService: DiagnosticsService[Task] =
      new GrpcDiagnosticsService(conf.grpcHost(), conf.grpcPort())
    implicit val deployService: DeployService[Task] =
      new GrpcDeployService(conf.grpcHost(), conf.grpcPort())

    val exec: Task[Unit] = conf.eval.toOption match {
      case Some(fileName) => {
        implicit val consoleIO: ConsoleIO[Task] = new effects.JLineConsoleIO(createConsole)
        new ReplRuntime(conf).evalProgram[Task](fileName)
      }
      case None if (conf.repl()) => {
        implicit val consoleIO: ConsoleIO[Task] = new effects.JLineConsoleIO(createConsole)
        new ReplRuntime(conf).replProgram[Task].as(())
      }
      case None if (conf.diagnostics()) => {
        implicit val consoleIO: ConsoleIO[Task] = new effects.JLineConsoleIO(createConsole)
        DiagnosticsRuntime.diagnosticsProgram[Task]
      }
      case None if (conf.deployDemo()) => DeployRuntime.deployProgram[Task]
      case None =>
        new NodeRuntime(conf).nodeProgram.value.map {
          case Right(_) => ()
          case Left(CouldNotConnectToBootstrap) =>
            Task.delay(println("Node could not connect to bootstrap node."))
          case Left(error) => Task.delay(println(s"Failed! Reason: '$error"))
        }
    }
    exec.unsafeRunSync
  }

  private def createConsole: ConsoleReader = {
    val console = new ConsoleReader()
    console.setHistoryEnabled(true)
    console.setPrompt("rholang $ ".green)
    console.addCompleter(new StringsCompleter(ReplRuntime.keywords.asJava))
    console
  }

}
