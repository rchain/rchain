package coop.rchain.node

import java.security.Security

import coop.rchain.shared.StringOps._
import cats.implicits._
import scala.tools.jline.console._, completer.StringsCompleter
import scala.collection.JavaConverters._

import coop.rchain.comm._
import coop.rchain.casper.util.comm.{DeployRuntime, DeployService, GrpcDeployService}
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

    Security.insertProviderAt(new BouncyCastleProvider(), 1)

    implicit val io: SchedulerService = Scheduler.io("repl-io")

    implicit val replService: ReplService[Task] =
      new GrpcReplService(conf.grpcHost(), conf.grpcPort())
    implicit val diagnosticsService: diagnostics.client.DiagnosticsService[Task] =
      new diagnostics.client.GrpcDiagnosticsService(conf.grpcHost(), conf.grpcPort())
    implicit val deployService: DeployService[Task] =
      new GrpcDeployService(conf.grpcHost(), conf.grpcPort())

    val exec: Task[Unit] = conf.eval.toOption match {
      case Some(fileName) => {
        implicit val consoleIO: ConsoleIO[Task] = effects.consoleIO(createConsole)
        new ReplRuntime(conf).evalProgram[Task](fileName)
      }
      case None if conf.repl() => {
        implicit val consoleIO: ConsoleIO[Task] = effects.consoleIO(createConsole)
        new ReplRuntime(conf).replProgram[Task].as(())
      }
      case None if conf.diagnostics() => {
        implicit val consoleIO: ConsoleIO[Task] = effects.consoleIO(createConsole)
        diagnostics.client.Runtime.diagnosticsProgram[Task]
      }
      case None if conf.deploy.toOption.isDefined =>
        DeployRuntime.deployFileProgram[Task](conf.deploy.toOption.get)
      case None if conf.deployDemo() => DeployRuntime.deployDemoProgram[Task]
      case None if conf.propose() =>
        conf.secretKey.toOption match {
          case Some(sk) => DeployRuntime.propose[Task](Base16.decode(sk))
          case None =>
            Task.delay {
              println("Error: value of --secret-key must be specified to propose a block")
            }
        }
      case None if conf.showBlock.toOption.isDefined =>
        DeployRuntime.showBlock[Task](conf.showBlock.toOption.get)
      case None =>
        new NodeRuntime(conf).nodeProgram.value.map {
          case Right(_) => ()
          case Left(CouldNotConnectToBootstrap) =>
            println("Node could not connect to bootstrap node.")
          case Left(error) => println(s"Failed! Reason: '$error")
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
