package coop.rchain.node

import java.security.Security

import scala.collection.JavaConverters._
import scala.tools.jline.console._
import completer.StringsCompleter

import cats.implicits._

import coop.rchain.casper.util.comm._
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.comm._
import coop.rchain.node.configuration._
import coop.rchain.node.effects._
import coop.rchain.shared.{Log, LogSource}
import coop.rchain.shared.StringOps._

import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService
import org.bouncycastle.jce.provider.BouncyCastleProvider

object Main {

  private implicit val logSource: LogSource = LogSource(this.getClass)
  private implicit val log: Log[Task]       = effects.log
  private implicit val io: SchedulerService = Scheduler.io("repl-io")

  def main(args: Array[String]): Unit = {
    Security.insertProviderAt(new BouncyCastleProvider(), 1)

    val exec: Task[Unit] =
      for {
        conf <- Configuration(args)
        _    <- mainProgram(conf)
      } yield ()

    exec.unsafeRunSync
  }

  private def mainProgram(conf: Configuration): Task[Unit] = {
    implicit val replService: ReplClient[Task] =
      new GrpcReplClient(conf.grpcServer.host, conf.grpcServer.port)
    implicit val diagnosticsService: diagnostics.client.DiagnosticsService[Task] =
      new diagnostics.client.GrpcDiagnosticsService(conf.grpcServer.host, conf.grpcServer.port)
    implicit val deployService: DeployService[Task] =
      new GrpcDeployService(conf.grpcServer.host, conf.grpcServer.port)

    conf.command match {
      case Eval(files)      => new ReplRuntime().evalProgram[Task](files)
      case Repl             => new ReplRuntime().replProgram[Task].as(())
      case Diagnostics      => diagnostics.client.Runtime.diagnosticsProgram[Task]
      case Deploy(location) => DeployRuntime.deployFileProgram[Task](location)
      case DeployDemo       => DeployRuntime.deployDemoProgram[Task]
      case Propose          => DeployRuntime.propose[Task]()
      case ShowBlock(hash)  => DeployRuntime.showBlock[Task](hash)
      case ShowBlocks       => DeployRuntime.showBlocks[Task]()
      case Run              => nodeProgram(conf)
      case _                => Task.delay(conf.printHelp())
    }
  }

  private def nodeProgram(conf: Configuration): Task[Unit] =
    for {
      host   <- conf.fetchHost
      result <- new NodeRuntime(conf, host).main.value
      _ <- result match {
            case Right(_) =>
              Task.unit
            case Left(CouldNotConnectToBootstrap) =>
              log.error("Node could not connect to bootstrap node.")
            case Left(error) =>
              log.error(s"Failed! Reason: '$error")
          }
    } yield ()

  implicit private def consoleIO: ConsoleIO[Task] = {
    val console = new ConsoleReader()
    console.setHistoryEnabled(true)
    console.setPrompt("rholang $ ".green)
    console.addCompleter(new StringsCompleter(ReplRuntime.keywords.asJava))
    effects.consoleIO(console)
  }

}
