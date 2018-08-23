package coop.rchain.node

import java.security.Security
import java.util.concurrent.{SynchronousQueue, ThreadPoolExecutor, TimeUnit}

import scala.collection.JavaConverters._
import scala.tools.jline.console._
import completer.StringsCompleter
import cats.implicits._
import coop.rchain.casper.util.comm._
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.comm._
import coop.rchain.node.configuration._
import coop.rchain.node.diagnostics.client.GrpcDiagnosticsService
import coop.rchain.node.effects._
import coop.rchain.shared.{Log, LogSource}
import coop.rchain.shared.StringOps._
import monix.eval.Task
import monix.execution.{Scheduler, UncaughtExceptionReporter}
import monix.execution.UncaughtExceptionReporter.LogExceptionsToStandardErr
import monix.execution.schedulers.{ExecutorScheduler, SchedulerService, ThreadFactoryBuilder, _}

object Main {

  private implicit val logSource: LogSource = LogSource(this.getClass)
  private implicit val log: Log[Task]       = effects.log
  //TODO @pawel make this a flag! now!
  //TODO create separate scheduler for casper
  private implicit val io: SchedulerService = Scheduler.fixedPool("repl-io", 3999)

  def main(args: Array[String]): Unit = {

    val exec: Task[Unit] =
      for {
        conf <- Configuration(args)
        _    <- mainProgram(conf)
      } yield ()

    exec.unsafeRunSync
  }

  private def mainProgram(conf: Configuration): Task[Unit] = {
    implicit val replService: GrpcReplClient =
      new GrpcReplClient(conf.grpcServer.host, conf.grpcServer.portInternal)
    implicit val diagnosticsService: GrpcDiagnosticsService =
      new diagnostics.client.GrpcDiagnosticsService(conf.grpcServer.host,
                                                    conf.grpcServer.portInternal)
    implicit val deployService: GrpcDeployService =
      new GrpcDeployService(conf.grpcServer.host, conf.grpcServer.portExternal)

    val program = conf.command match {
      case Eval(files) => new ReplRuntime().evalProgram[Task](files)
      case Repl        => new ReplRuntime().replProgram[Task].as(())
      case Diagnostics => diagnostics.client.Runtime.diagnosticsProgram[Task]
      case Deploy(address, phlo, phloPrice, nonce, location) =>
        DeployRuntime.deployFileProgram[Task](address, phlo, phloPrice, nonce, location)
      case DeployDemo      => DeployRuntime.deployDemoProgram[Task]
      case Propose         => DeployRuntime.propose[Task]()
      case ShowBlock(hash) => DeployRuntime.showBlock[Task](hash)
      case ShowBlocks      => DeployRuntime.showBlocks[Task]()
      case Run             => nodeProgram(conf)
      case _               => conf.printHelp()
    }

    program.doOnFinish(_ =>
      Task.delay {
        replService.close()
        diagnosticsService.close()
        deployService.close()
    })
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
