package coop.rchain.node

import java.nio.file.{Path, Paths}
import java.security.Security

import coop.rchain.shared.StringOps._
import cats.implicits._

import scala.tools.jline.console._
import completer.StringsCompleter
import scala.collection.JavaConverters._
import coop.rchain.comm._
import coop.rchain.casper.util.comm.{DeployRuntime, DeployService, GrpcDeployService}
import coop.rchain.node.effects.{ConsoleIO, GrpcReplClient, ReplClient}
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib._
import coop.rchain.crypto.codec.Base16
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService

import scala.util.{Failure, Success, Try}
import org.bouncycastle.jce.provider.BouncyCastleProvider

object Main {

  def main(args: Array[String]): Unit = {
    val conf = Conf(args)
    println(s"Starting with profile ${conf.profile.toOption.map(_.name)}")

    Security.insertProviderAt(new BouncyCastleProvider(), 1)

    implicit val io: SchedulerService = Scheduler.io("repl-io")

    implicit val replService: ReplClient[Task] =
      new GrpcReplClient(conf.grpcHost(), conf.grpcPort())
    implicit val diagnosticsService: diagnostics.client.DiagnosticsService[Task] =
      new diagnostics.client.GrpcDiagnosticsService(conf.grpcHost(), conf.grpcPort())
    implicit val deployService: DeployService[Task] =
      new GrpcDeployService(conf.grpcHost(), conf.grpcPort())

    val exec: Task[Unit] = conf.subcommand match {
      case Some(conf.eval) => {
        implicit val consoleIO: ConsoleIO[Task] = effects.consoleIO(createConsole)
        new ReplRuntime(conf).evalProgram[Task](conf.eval.fileNames.toOption.get)
      }
      case Some(conf.repl) => {
        implicit val consoleIO: ConsoleIO[Task] = effects.consoleIO(createConsole)
        new ReplRuntime(conf).replProgram[Task].as(())
      }
      case Some(conf.diagnostics) => {
        implicit val consoleIO: ConsoleIO[Task] = effects.consoleIO(createConsole)
        diagnostics.client.Runtime.diagnosticsProgram[Task]
      }
      case Some(conf.deploy) =>
        DeployRuntime.deployFileProgram[Task](conf.deploy.location.toOption.get)

      case Some(conf.deployDemo) => DeployRuntime.deployDemoProgram[Task]
      case Some(conf.propose)    => DeployRuntime.propose[Task]()
      case Some(conf.showBlock) =>
        DeployRuntime.showBlock[Task](conf.showBlock.hash.toOption.get)
      case Some(conf.showBlocks) =>
        DeployRuntime.showBlocks[Task]()
      case Some(conf.run) =>
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
