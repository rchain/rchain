package coop.rchain.node

import coop.rchain.shared.StringOps._
import cats.implicits._
import scala.tools.jline.console._, completer.StringsCompleter
import scala.collection.JavaConverters._

import coop.rchain.comm._
import coop.rchain.casper.util.comm.{DeployRuntime, DeployService, GrpcDeployService}
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib._
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService
import scala.util.{Failure, Success, Try}

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

    val certificate = conf.certificate.map(c => Try(CertificateHelper.from(c))).toOption match {
      case Some(Success(c)) if CertificateHelper.isSecp256k1(c) => Some(c)
      case Some(Success(_)) =>
        println("Certificate must contain a secp256k1 EC Public Key")
        System.exit(1)
        None
      case Some(Failure(e)) =>
        println(s"Failed to read the X.509 certificate: ${e.getMessage}")
        System.exit(1)
        None
      case _ => None
    }

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
      case None if (conf.deploy.toOption.isDefined) =>
        DeployRuntime.deployFileProgram[Task](conf.deploy.toOption.get)
      case None if (conf.deployDemo()) => DeployRuntime.deployDemoProgram[Task]
      case None if (conf.propose())    => DeployRuntime.forcePropose[Task]
      case None if (conf.showBlock.toOption.isDefined) =>
        DeployRuntime.showBlock[Task](conf.showBlock.toOption.get)
      case None =>
        certificate
          .flatMap(CertificateHelper.publicAddress)
          .map { name =>
            new NodeRuntime(conf, name).nodeProgram.value.map {
              case Right(_) => ()
              case Left(CouldNotConnectToBootstrap) =>
                println("Node could not connect to bootstrap node.")
              case Left(error) => println(s"Failed! Reason: '$error")
            }
          }
          .getOrElse(Task.eval(println("No server certificate provided")))
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
