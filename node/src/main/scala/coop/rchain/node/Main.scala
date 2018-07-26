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
import coop.rchain.shared.StringOps._

import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService
import org.bouncycastle.jce.provider.BouncyCastleProvider

object Main {

  def main(args: Array[String]): Unit = {
    val conf = NodeConfiguration(args)
    println(s"Starting with profile ${conf.profile}")

    Security.insertProviderAt(new BouncyCastleProvider(), 1)

    implicit val io: SchedulerService = Scheduler.io("repl-io")

    implicit val replService: ReplClient[Task] =
      new GrpcReplClient(conf.grpcServer.host, conf.grpcServer.port)
    implicit val diagnosticsService: diagnostics.client.DiagnosticsService[Task] =
      new diagnostics.client.GrpcDiagnosticsService(conf.grpcServer.host, conf.grpcServer.port)
    implicit val deployService: DeployService[Task] =
      new GrpcDeployService(conf.grpcServer.host, conf.grpcServer.port)

    val exec: Task[Unit] = conf.command match {
      case Eval(files) =>
        implicit val consoleIO: ConsoleIO[Task] = effects.consoleIO(createConsole)
        new ReplRuntime().evalProgram[Task](files)

      case Repl =>
        implicit val consoleIO: ConsoleIO[Task] = effects.consoleIO(createConsole)
        new ReplRuntime().replProgram[Task].as(())

      case Diagnostics =>
        implicit val consoleIO: ConsoleIO[Task] = effects.consoleIO(createConsole)
        diagnostics.client.Runtime.diagnosticsProgram[Task]

      case Deploy(location) => DeployRuntime.deployFileProgram[Task](location)

      case DeployDemo => DeployRuntime.deployDemoProgram[Task]

      case Propose => DeployRuntime.propose[Task]()

      case ShowBlock(hash) => DeployRuntime.showBlock[Task](hash)

      case ShowBlocks => DeployRuntime.showBlocks[Task]()

      case Run =>
        new NodeRuntime(conf).nodeProgram.value.map {
          case Right(_) => ()
          case Left(CouldNotConnectToBootstrap) =>
            println("Node could not connect to bootstrap node.")
          case Left(error) => println(s"Failed! Reason: '$error")
        }

      case _ => Task.delay(conf.printHelp())
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
