package coop.rchain.node

import scala.collection.JavaConverters._
import scala.tools.jline.console._
import completer.StringsCompleter

import cats.implicits._

import coop.rchain.casper.util.comm._
import coop.rchain.casper.util.BondingUtil
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
import coop.rchain.node.configuration._
import coop.rchain.node.effects._
import coop.rchain.shared._
import coop.rchain.shared.StringOps._

import monix.eval.Task
import monix.execution.Scheduler
import org.slf4j.LoggerFactory
import org.slf4j.bridge.SLF4JBridgeHandler

object Main {

  implicit private val logSource: LogSource     = LogSource(this.getClass)
  implicit private val log: Log[Task]           = effects.log
  implicit private val eventLog: EventLog[Task] = EventLog.eventLogger

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def main(args: Array[String]): Unit = {
    // Catch-all for unhandled exceptions. Use only JDK and SLF4J.
    Thread.setDefaultUncaughtExceptionHandler((thread, ex) => {
      LoggerFactory.getLogger(getClass).error("Unhandled exception in thread " + thread.getName, ex)
      ex.printStackTrace()
    })

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
      case Deploy(phlo, phloPrice, validAfterBlock, privateKey, location) =>
        DeployRuntime
          .deployFileProgram[Task](
            phlo,
            phloPrice,
            validAfterBlock,
            privateKey,
            location
          )
      case Propose           => DeployRuntime.propose[Task]()
      case ShowBlock(hash)   => DeployRuntime.getBlock[Task](hash)
      case ShowBlocks(depth) => DeployRuntime.getBlocks[Task](depth)
      case VisualizeDag(depth, showJustificationLines) =>
        DeployRuntime.visualizeDag[Task](depth, showJustificationLines)
      case MachineVerifiableDag => DeployRuntime.machineVerifiableDag[Task]
      case DataAtName(name)     => DeployRuntime.listenForDataAtName[Task](name)
      case ContAtName(names)    => DeployRuntime.listenForContinuationAtName[Task](names)
      case Keygen(algorithm)    => generateKey(conf, algorithm)
      case Run                  => nodeProgram(conf)
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
          deployService.close()
        }
    )
  }

  private def generateKey(conf: Configuration, algorithm: String): Task[Unit] =
    for {
      keyPair <- algorithm.toLowerCase match {
                  case Ed25519.name   => Ed25519.newKeyPair.pure[Task]
                  case Secp256k1.name => Secp256k1.newKeyPair.pure[Task]
                  case _              => Task.raiseError(new IllegalStateException("Invalid algorithm name"))
                }
      (sec, pub) = keyPair
      sk         = Base16.encode(sec.bytes)
      pk         = Base16.encode(pub.bytes)
      _ <- ConsoleIO[Task].println(s"Generated public key: $pk") >>
            ConsoleIO[Task].println(s"Generated private key: $sk")
    } yield ()

  private def nodeProgram(conf: Configuration)(implicit scheduler: Scheduler): Task[Unit] = {
    // XXX: Enable it earlier once we have JDK with https://bugs.openjdk.java.net/browse/JDK-8218960 fixed
    // https://www.slf4j.org/legacy.html#jul-to-slf4j
    SLF4JBridgeHandler.removeHandlersForRootLogger()
    SLF4JBridgeHandler.install()
    for {
      _       <- checkPorts(conf)
      _       <- log.info(VersionInfo.get)
      _       <- logConfiguration(conf)
      runtime <- NodeRuntime(conf)
      _       <- runtime.main
    } yield ()
  }

  private def checkPorts(conf: Configuration): Task[Unit] = {
    def isLocalPortAvailable(port: Int): Task[Boolean] =
      Task
        .delay(new java.net.ServerSocket(port).close())
        .attempt
        .map(_.isRight)
        .ifM(
          true.pure[Task],
          log.error(s"Port $port is already in use!").as(false)
        )

    List(
      conf.server.port,
      conf.server.kademliaPort,
      conf.server.httpPort,
      conf.grpcServer.portExternal,
      conf.grpcServer.portInternal
    ).traverse(isLocalPortAvailable)
      .map(_.forall(identity))
      .ifM(Task.unit, Task.delay(System.exit(1)))
  }

  private def logConfiguration(conf: Configuration): Task[Unit] =
    Task
      .sequence(
        Seq(
          log.info(s"Starting with profile ${conf.profile}"),
          log.info(s"Using configuration file: ${conf.configurationFile}"),
          if (!conf.configurationFile.toFile.exists()) log.warn("Configuration file not found!")
          else Task.unit,
          log.info(s"Running on network: ${conf.server.networkId}")
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
