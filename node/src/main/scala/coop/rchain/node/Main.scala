package coop.rchain.node

import scala.collection.JavaConverters._
import scala.tools.jline.console._
import completer.StringsCompleter
import cats.implicits._
import coop.rchain.casper.util.comm._
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.casper.util.BondingUtil
import coop.rchain.comm._
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
import coop.rchain.node.configuration._
import coop.rchain.node.diagnostics.client.GrpcDiagnosticsService
import coop.rchain.node.effects._
import coop.rchain.shared._
import coop.rchain.shared.StringOps._
import monix.eval.Task
import monix.execution.Scheduler

object Main {

  private implicit val logSource: LogSource = LogSource(this.getClass)
  private implicit val log: Log[Task]       = effects.log

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def main(args: Array[String]): Unit = {

    val configuration = Configuration.build(args)
    System.setProperty("rnode.data.dir", configuration.server.dataDir.toString) // NonUnitStatements

    implicit val scheduler: Scheduler = Scheduler.computation(
      Math.max(java.lang.Runtime.getRuntime.availableProcessors(), 2),
      "node-runner",
      reporter = UncaughtExceptionLogger
    )

    Task.defer(mainProgram(configuration)).unsafeRunSync
  }

  private def mainProgram(conf: Configuration)(implicit scheduler: Scheduler): Task[Unit] = {
    implicit val replService: GrpcReplClient =
      new GrpcReplClient(
        conf.grpcServer.host,
        conf.grpcServer.portInternal,
        conf.grpcServer.maxMessageSize
      )
    implicit val diagnosticsService: GrpcDiagnosticsService =
      new diagnostics.client.GrpcDiagnosticsService(
        conf.grpcServer.host,
        conf.grpcServer.portInternal,
        conf.grpcServer.maxMessageSize
      )
    implicit val deployService: GrpcDeployService =
      new GrpcDeployService(
        conf.grpcServer.host,
        conf.grpcServer.portExternal,
        conf.grpcServer.maxMessageSize
      )

    implicit val time: Time[Task] = effects.time

    val program = conf.command match {
      case Eval(files) => new ReplRuntime().evalProgram[Task](files)
      case Repl        => new ReplRuntime().replProgram[Task].as(())
      case Diagnostics => diagnostics.client.Runtime.diagnosticsProgram[Task]
      case Deploy(address, phlo, phloPrice, validAfterBlock, userId, location) =>
        DeployRuntime
          .deployFileProgram[Task](
            address,
            phlo,
            phloPrice,
            validAfterBlock,
            userId,
            location
          )
      case DeployDemo        => DeployRuntime.deployDemoProgram[Task]
      case Propose           => DeployRuntime.propose[Task]()
      case ShowBlock(hash)   => DeployRuntime.showBlock[Task](hash)
      case ShowBlocks(depth) => DeployRuntime.showBlocks[Task](depth)
      case VisualizeDag(depth, showJustificationLines) =>
        DeployRuntime.visualizeDag[Task](depth, showJustificationLines)
      case DataAtName(name)  => DeployRuntime.listenForDataAtName[Task](name)
      case ContAtName(names) => DeployRuntime.listenForContinuationAtName[Task](names)
      case Run               => nodeProgram(conf)
      case BondingDeployGen(bondKey, ethAddress, amount, secKey, pubKey) =>
        implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
        BondingUtil
          .writeIssuanceBasedRhoFiles[Task, Task.Par](bondKey, ethAddress, amount, secKey, pubKey)
      case FaucetBondingDeployGen(amount, sigAlgorithm, secKey, pubKey) =>
        implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
        BondingUtil.writeFaucetBasedRhoFiles[Task, Task.Par](amount, sigAlgorithm, secKey, pubKey)
      case _ => conf.printHelp()
    }

    program.doOnFinish(
      _ =>
        Task.delay {
          replService.close()
          diagnosticsService.close()
          deployService.close()
        }
    )
  }

  private def nodeProgram(conf: Configuration)(implicit scheduler: Scheduler): Task[Unit] = {
    val node =
      for {
        _       <- log.info(VersionInfo.get).toEffect
        _       <- logConfiguration(conf).toEffect
        runtime <- NodeRuntime(conf)
        _       <- runtime.main
      } yield ()

    node.value >>= {
      case Right(_) =>
        Task.unit
      case Left(CouldNotConnectToBootstrap) =>
        log.error("Node could not connect to bootstrap node.")
      case Left(InitializationError(msg)) =>
        log.error(msg) >>
          Task.delay(System.exit(-1))
      case Left(error) =>
        log.error(s"Failed! Reason: '$error")
    }
  }

  private def logConfiguration(conf: Configuration): Task[Unit] =
    Task
      .sequence(
        Seq(
          log.info(s"Starting with profile ${conf.profile}"),
          log.info(s"Using configuration file: ${conf.configurationFile}"),
          if (!conf.configurationFile.toFile.exists()) log.warn("Configuration file not found!")
          else Task.unit
        )
      )
      .void

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  implicit private def consoleIO: ConsoleIO[Task] = {
    val console = new ConsoleReader()
    console.setHistoryEnabled(true)
    if (TerminalMode.readMode)
      console.setPrompt("rholang $ ".green.colorize)
    console.addCompleter(new StringsCompleter(ReplRuntime.keywords.asJava))
    effects.consoleIO(console)
  }

}
