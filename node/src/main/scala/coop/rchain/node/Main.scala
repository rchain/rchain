package coop.rchain.node

import java.nio.file.Path

import cats.implicits._
import cats.effect.Resource
import coop.rchain.casper.util.comm._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.{Secp256k1, SignaturesAlg}
import coop.rchain.crypto.util.KeyUtil
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
import coop.rchain.node.configuration._
import coop.rchain.node.effects._
import coop.rchain.shared.StringOps._
import coop.rchain.shared._
import monix.eval.Task
import monix.execution.Scheduler
import org.slf4j.LoggerFactory
import org.slf4j.bridge.SLF4JBridgeHandler

import scala.collection.JavaConverters._
import scala.tools.jline.console._
import scala.tools.jline.console.completer.StringsCompleter

object Main {
  private val RNodeDeployerPasswordEnvVar = "RNODE_DEPLOYER_PASSWORD"

  implicit private val logSource: LogSource     = LogSource(this.getClass)
  implicit private val log: Log[Task]           = effects.log
  implicit private val eventLog: EventLog[Task] = EventLog.eventLogger

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def main(args: Array[String]): Unit = {
    // Catch-all for unhandled exceptions. Use only JDK and SLF4J.
    Thread.setDefaultUncaughtExceptionHandler((thread, ex) => {
      LoggerFactory.getLogger(getClass).error("Unhandled exception in thread " + thread.getName, ex)
    })

    val configuration = Configuration.build(args)
    System.setProperty("rnode.data.dir", configuration.server.dataDir.toString) // NonUnitStatements

    implicit val scheduler: Scheduler = Scheduler.computation(
      Math.max(java.lang.Runtime.getRuntime.availableProcessors(), 2),
      "node-runner",
      reporter = UncaughtExceptionLogger
    )

    Task
      .defer(logUnknownConfigurationKeys(configuration) >> mainProgram(configuration))
      .unsafeRunSync
  }

  private def logUnknownConfigurationKeys(conf: Configuration): Task[Unit] =
    conf.unknownConfigKeys.toList
      .traverse(k => log.warn(s"Unknown configuration key $k"))
      .void

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
    implicit val proposeService: GrpcProposeService =
      new GrpcProposeService(
        conf.grpcServer.host,
        conf.grpcServer.portInternal,
        conf.grpcServer.maxMessageSize
      )

    implicit val time: Time[Task] = effects.time

    implicit val envVars: EnvVars[Task] = EnvVars.envVars

    implicit val console: ConsoleIO[Task] = consoleIO

    val program = conf.command match {
      case Eval(files, printUnmatchedSendsOnly) =>
        new ReplRuntime().evalProgram[Task](files, printUnmatchedSendsOnly)
      case Repl => new ReplRuntime().replProgram[Task].as(())
      case Deploy(
          phlo,
          phloPrice,
          validAfterBlock,
          maybePrivateKey,
          maybePrivateKeyPath,
          location
          ) =>
        def decryptPrivateKey(encryptedPrivateKeyPath: Path): Task[PrivateKey] =
          for {
            maybePassword <- EnvVars[Task].get(RNodeDeployerPasswordEnvVar)
            password <- maybePassword
                         .map(p => p.pure[Task])
                         .getOrElse(ConsoleIO[Task].readPassword("Password for private key file: "))

            privateKey <- Secp256k1.parsePemFile[Task](encryptedPrivateKeyPath, password)
          } yield privateKey

        val getPrivateKey =
          maybePrivateKey
            .map(_.pure[Task])
            .orElse(maybePrivateKeyPath.map(decryptPrivateKey))
            .sequence

        for {
          privateKey <- getPrivateKey
          _ <- DeployRuntime.deployFileProgram[Task](
                phlo,
                phloPrice,
                validAfterBlock,
                privateKey,
                location
              )
        } yield ()
      case FindDeploy(deployId)         => DeployRuntime.findDeploy[Task](deployId)
      case Propose(printUnmatchedSends) => DeployRuntime.propose[Task](printUnmatchedSends)
      case ShowBlock(hash)              => DeployRuntime.getBlock[Task](hash)
      case ShowBlocks(depth)            => DeployRuntime.getBlocks[Task](depth)
      case VisualizeDag(depth, showJustificationLines) =>
        DeployRuntime.visualizeDag[Task](depth, showJustificationLines)
      case MachineVerifiableDag              => DeployRuntime.machineVerifiableDag[Task]
      case DataAtName(name)                  => DeployRuntime.listenForDataAtName[Task](name)
      case ContAtName(names)                 => DeployRuntime.listenForContinuationAtName[Task](names)
      case Keygen(algorithm, privateKeyPath) => generateKey(conf, algorithm, privateKeyPath)
      case LastFinalizedBlock                => DeployRuntime.lastFinalizedBlock[Task]
      case Run                               => nodeProgram(conf)
      case _                                 => conf.printHelp()
    }

    Task.delay(
      sys.addShutdownHook {
        proposeService.close()
        replService.close()
        deployService.close()
        console.close.unsafeRunSync(scheduler)
      }
    ) >> program
  }

  private def generateKey(
      conf: Configuration,
      algorithm: String,
      privateKeyPath: Path
  )(implicit console: ConsoleIO[Task]): Task[Unit] =
    for {
      password       <- ConsoleIO[Task].readPassword("Password for generated private key file: ")
      passwordRepeat <- ConsoleIO[Task].readPassword("Repeat password: ")
      _ <- if (password != passwordRepeat) {
            ConsoleIO[Task].println("Passwords do not match. Try again.") >> generateKey(
              conf,
              algorithm,
              privateKeyPath
            )
          } else if (password.isEmpty) {
            ConsoleIO[Task].println("Password is empty. Try again.") >> generateKey(
              conf,
              algorithm,
              privateKeyPath
            )
          } else {
            for {
              sigAlgorithm <- SignaturesAlg(algorithm)
                               .map(_.pure[Task])
                               .getOrElse(
                                 Task
                                   .raiseError(new IllegalStateException("Invalid algorithm name"))
                               )
              (sk, _) = sigAlgorithm.newKeyPair
              _       <- KeyUtil.writePrivateKey(sk, sigAlgorithm, password, privateKeyPath)
              _ <- ConsoleIO[Task].println(
                    s"Successfully generated private key: ${privateKeyPath.toAbsolutePath}"
                  )
            } yield ()
          }
    } yield ()

  private def nodeProgram(conf: Configuration)(implicit scheduler: Scheduler): Task[Unit] = {
    // XXX: Enable it earlier once we have JDK with https://bugs.openjdk.java.net/browse/JDK-8218960 fixed
    // https://www.slf4j.org/legacy.html#jul-to-slf4j
    SLF4JBridgeHandler.removeHandlersForRootLogger()
    SLF4JBridgeHandler.install()
    for {
      _             <- checkHost(conf)
      confWithPorts <- checkPorts(conf)
      _             <- log.info(VersionInfo.get)
      _             <- logConfiguration(confWithPorts)
      runtime       <- NodeRuntime(confWithPorts)
      _             <- runtime.main
    } yield ()
  }

  private def checkHost(conf: Configuration): Task[Unit] = {
    import coop.rchain.comm._
    conf.server.host.fold(Task.unit) { h =>
      Task
        .now(conf.server.allowPrivateAddresses)
        .ifM(isValidInetAddress[Task](h), isValidPublicInetAddress[Task](h))
        .ifM(
          Task.unit,
          for {
            _ <- log.error(
                  s"Kademlia hostname '$h' is not valid or it does not resolve to a public IP address"
                )
            _ <- log.error("Hint: Run me with --allow-private-addresses in private networks")
            _ <- Task.raiseError[Unit](new Exception("Invalid Kademlia hostname"))
          } yield ()
        )
    }
  }

  private def checkPorts(conf: Configuration): Task[Configuration] = {
    def getFreePort: Task[Int] =
      Resource
        .fromAutoCloseable(
          Task.delay(new java.net.ServerSocket(0))
        )
        .use { socket =>
          Task.delay {
            socket.setReuseAddress(true)
            socket.getLocalPort
          }
        }

    def isLocalPortAvailable(port: Int): Task[Boolean] =
      Task
        .delay(new java.net.ServerSocket(port).close())
        .attempt
        .map(_.isRight)
        .ifM(
          true.pure[Task],
          log.error(s"Port $port is already in use!").as(false)
        )

    def checkRChainProtocolPort(configuration: Configuration): Task[Configuration] =
      isLocalPortAvailable(configuration.server.port).ifM(
        configuration.pure[Task],
        if (configuration.server.useRandomPorts)
          for {
            port <- getFreePort
            _    <- log.info(s"Using random port $port as RChain Protocol port")
          } yield configuration.copy(server = configuration.server.copy(port = port))
        else
          log.error(
            "Hint: Run me with --use-random-ports to use a random port for RChain Protocol port"
          ) >> Task.raiseError(new Exception("Invalid RChain Protocol port"))
      )

    def checkKademliaPort(configuration: Configuration): Task[Configuration] =
      isLocalPortAvailable(configuration.server.kademliaPort).ifM(
        configuration.pure[Task],
        if (configuration.server.useRandomPorts)
          for {
            port <- getFreePort
            _    <- log.info(s"Using random port $port as Kademlia port")
          } yield configuration.copy(server = configuration.server.copy(kademliaPort = port))
        else
          log.error(
            "Hint: Run me with --use-random-ports to use a random port for Kademlia port"
          ) >> Task.raiseError(new Exception("Invalid Kademlia port"))
      )

    for {
      portsAvailability <- List(
                            ("http", conf.server.httpPort),
                            ("grpc server external", conf.grpcServer.portExternal),
                            ("grpc server internal", conf.grpcServer.portInternal)
                          ).traverse { _.traverse(isLocalPortAvailable) }

      unavailablePorts = portsAvailability.filter(!_._2).map(_._1)

      _ <- if (unavailablePorts.isEmpty)
            Task.unit
          else
            Task.raiseError(
              new Exception(
                s"Required ports are already in use: ${unavailablePorts.mkString(", ")}"
              )
            )

      rpPortConf       <- checkRChainProtocolPort(conf)
      kademliaPortConf <- checkKademliaPort(rpPortConf)
    } yield kademliaPortConf
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
  private def consoleIO: ConsoleIO[Task] = {
    val console = new ConsoleReader()
    console.setHistoryEnabled(true)
    if (TerminalMode.readMode)
      console.setPrompt("rholang $ ".green.colorize)
    console.addCompleter(new StringsCompleter(ReplRuntime.keywords.asJava))
    effects.consoleIO(console)
  }

}
