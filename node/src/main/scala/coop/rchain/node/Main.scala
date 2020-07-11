package coop.rchain.node

import java.io.File
import java.nio.file.Path

import cats.implicits._
import cats.effect.{Resource, Sync}
import coop.rchain.casper.util.comm._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.{Secp256k1, SignaturesAlg}
import coop.rchain.crypto.util.KeyUtil
import coop.rchain.node.configuration.Configuration.Profile
import coop.rchain.node.configuration._
import coop.rchain.node.effects._
import coop.rchain.node.web.VersionInfo
import coop.rchain.shared.StringOps._
import coop.rchain.shared.{EnvVars, _}
import monix.eval.Task
import monix.execution.Scheduler
import org.slf4j.LoggerFactory
import org.slf4j.bridge.SLF4JBridgeHandler

import scala.collection.JavaConverters._
import scala.tools.jline.console._
import scala.tools.jline.console.completer.StringsCompleter

object Main {

  implicit private val logSource: LogSource     = LogSource(this.getClass)
  implicit private val log: Log[Task]           = effects.log
  implicit private val eventLog: EventLog[Task] = EventLog.eventLogger

  // Main scheduler for all CPU bounded tasks
  // Should always be passed as implicit dependency.
  // All other schedulers should be explicit.
  implicit val scheduler: Scheduler = Scheduler.computation(
    Math.max(java.lang.Runtime.getRuntime.availableProcessors(), 2),
    "node-runner",
    reporter = UncaughtExceptionLogger
  )

  /**
    * Main entry point
    * @param args input args
    */
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def main(args: Array[String]): Unit = {
    // Catch-all for unhandled exceptions. Use only JDK and SLF4J.
    Thread.setDefaultUncaughtExceptionHandler((thread, ex) => {
      LoggerFactory.getLogger(getClass).error("Unhandled exception in thread " + thread.getName, ex)
    })
    implicit val console: ConsoleIO[Task] = consoleIO

    // Ensure terminal is restored on exit
    sys.addShutdownHook {
      console.close.unsafeRunSync
    }

    // Parse CLI options
    val options = commandline.Options(args)
    if (options.subcommand.contains(options.run))
      // Start the node
      startNode(options).unsafeRunSync
    //or
    else
      // Execute CLI command
      Task.defer(runCLI(options)).unsafeRunSync
  }

  /**
    * Starts RNode instance
    * @param options command line options
    * @param console console
    */
  private def startNode(options: commandline.Options)(
      implicit
      console: ConsoleIO[Task]
  ): Task[Unit] = {
    // Create merged configuration from CLI options and config file
    val (nodeConf, profile, configFile, kamonConf) = Configuration.build(options)
    // This system variable is used in Logger config file `node/src/main/resources/logback.xml`
    val _ = System.setProperty("rnode.data.dir", nodeConf.storage.dataDir.toString)

    Task
      .defer({
        // and start node
        // TODO : Enable it earlier once we have JDK with https://bugs.openjdk.java.net/browse/JDK-8218960 fixed
        // https://www.slf4j.org/legacy.html#jul-to-slf4j
        SLF4JBridgeHandler.removeHandlersForRootLogger()
        SLF4JBridgeHandler.install()
        for {
          _               <- checkHost(nodeConf)
          confWithPorts   <- checkPorts(nodeConf)
          confWithDecrypt <- loadPrivateKeyFromFile(confWithPorts)
          _               <- log.info(VersionInfo.get)
          _               <- logConfiguration(confWithDecrypt, profile, configFile, options)
          runtime         <- NodeRuntime(confWithDecrypt, kamonConf)
          _               <- runtime.main.run(NodeCallCtx.init)
        } yield ()
      })
  }

  /**
    * Executes CLI commands
    * @param options command line options
    * @param console console
    */
  private def runCLI(options: commandline.Options)(
      implicit
      console: ConsoleIO[Task]
  ): Task[Unit] = {
    // Clients for executing gRPC calls on remote RNode instance
    implicit val replServiceClient: GrpcReplClient =
      new GrpcReplClient(
        options.grpcHost(),
        options.grpcPort(),
        options.grpcMaxRecvMessageSize()
      )
    implicit val deployServiceClient: GrpcDeployService =
      new GrpcDeployService(
        options.grpcHost(),
        options.grpcPort(),
        options.grpcMaxRecvMessageSize()
      )
    implicit val proposeServiceClient: GrpcProposeService =
      new GrpcProposeService(
        options.grpcHost(),
        options.grpcPort(),
        options.grpcMaxRecvMessageSize()
      )

    implicit val time: Time[Task] = effects.time

    val program = subcommand(options) match {
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
        val getPrivateKey =
          maybePrivateKey
            .map(_.pure[Task])
            .orElse(maybePrivateKeyPath.map(decryptKeyFromCon))
            .sequence
        for {
          privateKey <- getPrivateKey
          _ <- privateKey.fold(Task.raiseError[Unit](new Exception("Private key is missing")))(
                DeployRuntime.deployFileProgram[Task](
                  phlo,
                  phloPrice,
                  validAfterBlock,
                  _,
                  location
                )
              )
        } yield ()
      case FindDeploy(deployId)         => DeployRuntime.findDeploy[Task](deployId)
      case Propose(printUnmatchedSends) => DeployRuntime.propose[Task](printUnmatchedSends)
      case ShowBlock(hash)              => DeployRuntime.getBlock[Task](hash)
      case ShowBlocks(depth)            => DeployRuntime.getBlocks[Task](depth)
      case VisualizeDag(depth, showJustificationLines) =>
        DeployRuntime.visualizeDag[Task](depth, showJustificationLines)
      case MachineVerifiableDag  => DeployRuntime.machineVerifiableDag[Task]
      case DataAtName(name)      => DeployRuntime.listenForDataAtName[Task](name)
      case ContAtName(names)     => DeployRuntime.listenForContinuationAtName[Task](names)
      case Keygen(path)          => generateKey(path)
      case LastFinalizedBlock    => DeployRuntime.lastFinalizedBlock[Task]
      case IsFinalized(hash)     => DeployRuntime.isFinalized[Task](hash)
      case BondStatus(publicKey) => DeployRuntime.bondStatus[Task](publicKey)
      case _                     => Task.delay(options.printHelp())
    }

    Task.delay(
      sys.addShutdownHook {
        proposeServiceClient.close()
        replServiceClient.close()
        deployServiceClient.close()
      }
    ) >> program
  }

  private def subcommand(options: commandline.Options): Command =
    options.subcommand match {
      case Some(options.eval) =>
        Eval(options.eval.fileNames(), options.eval.printUnmatchedSendsOnly())
      case Some(options.repl) => Repl
      case Some(options.deploy) =>
        import options.deploy._
        Deploy(
          phloLimit(),
          phloPrice(),
          validAfterBlockNumber.getOrElse(-1L),
          privateKey.toOption,
          privateKeyPath.toOption,
          location()
        )
      case Some(options.findDeploy) => FindDeploy(options.findDeploy.deployId())
      case Some(options.propose)    => Propose(options.propose.printUnmatchedSends())
      case Some(options.showBlock)  => ShowBlock(options.showBlock.hash())
      case Some(options.showBlocks) =>
        import options.showBlocks._
        ShowBlocks(depth.getOrElse(1))
      case Some(options.visualizeBlocks) =>
        import options.visualizeBlocks._
        VisualizeDag(depth.getOrElse(-1), showJustificationLines.getOrElse(false))
      case Some(options.machineVerifiableDag) => MachineVerifiableDag
      case Some(options.run)                  => Run
      case Some(options.keygen)               => Keygen(options.keygen.location())
      case Some(options.lastFinalizedBlock)   => LastFinalizedBlock
      case Some(options.isFinalized)          => IsFinalized(options.isFinalized.hash())
      case Some(options.bondStatus)           => BondStatus(options.bondStatus.validatorPublicKey())
      case Some(options.dataAtName)           => DataAtName(options.dataAtName.name())
      case Some(options.contAtName)           => ContAtName(options.contAtName.name())
      case _                                  => Help
    }

  private def decryptKeyFromCon(
      encryptedPrivateKeyPath: Path
  )(implicit console: ConsoleIO[Task]): Task[PrivateKey] =
    for {
      password   <- getValidatorPassword
      privateKey <- Secp256k1.parsePemFile[Task](encryptedPrivateKeyPath, password)
    } yield privateKey

  private def generateKey(
      path: Path
  )(implicit console: ConsoleIO[Task]): Task[Unit] =
    for {
      password       <- ConsoleIO[Task].readPassword("Enter password for keyfile: ")
      passwordRepeat <- ConsoleIO[Task].readPassword("Repeat password: ")
      _ <- if (password != passwordRepeat) {
            ConsoleIO[Task].println("Passwords do not match. Try again:") >> generateKey(path)
          } else if (password.isEmpty) {
            ConsoleIO[Task].println("Password is empty. Try again:") >> generateKey(path)
          } else {
            for {
              sigAlgorithm <- SignaturesAlg(Secp256k1.name)
                               .map(_.pure[Task])
                               .getOrElse(
                                 Task
                                   .raiseError(new IllegalStateException("Invalid algorithm name"))
                               )
              (sk, pk) = sigAlgorithm.newKeyPair

              privatePemKeyPath = path.resolve("rnode.key")
              publicPemKeyPath  = path.resolve("rnode.pub.pem")
              publicKeyHexFile  = path.resolve("rnode.pub.hex")
              _ <- KeyUtil.writeKeys(
                    sk,
                    pk,
                    sigAlgorithm,
                    password,
                    privatePemKeyPath,
                    publicPemKeyPath,
                    publicKeyHexFile
                  )
              _ <- ConsoleIO[Task].println(
                    s"\nSuccess!\n" +
                      s"Private key file (encrypted PEM format):  ${privatePemKeyPath.toAbsolutePath}\n" +
                      s"Public  key file (PEM format):            ${publicPemKeyPath.toAbsolutePath}\n" +
                      s"Public  key file (HEX format):            ${publicKeyHexFile.toAbsolutePath}"
                  )
            } yield ()
          }
    } yield ()

  private val RNodeValidatorPasswordEnvVar = "RNODE_VALIDATOR_PASSWORD"

  /**
    * Reads validator password from RNODE_VALIDATOR_PASSWORD env variable, if not set - asks for console
    * @param console
    * @return
    */
  def getValidatorPassword(implicit console: ConsoleIO[Task]): Task[String] =
    sys.env.get(RNodeValidatorPasswordEnvVar) match {
      case Some(password) =>
        if (password.length > 0) password.pure[Task] else requestForPassword
      case None => requestForPassword
    }

  def requestForPassword(implicit console: ConsoleIO[Task]): Task[String] =
    console
      .readPassword(
        "Variable RNODE_VALIDATOR_PASSWORD is not set, please enter password for keyfile. \n" +
          "Password for keyfile: "
      )

  /**
    * Loads validator key from file into configuration.
    * If key file is supplied by user, it overrides plain text private key
    * @param conf Node configuration instance
    * @param console
    * @return
    */
  private def loadPrivateKeyFromFile(
      conf: NodeConf
  )(implicit console: ConsoleIO[Task]): Task[NodeConf] =
    conf.casper.validatorPrivateKeyPath match {
      case Some(privateKeyPath) =>
        for {
          privateKeyBase16 <- decryptKeyFromCon(privateKeyPath).map(sk => Base16.encode(sk.bytes))
        } yield conf.copy(casper = conf.casper.copy(validatorPrivateKey = Some(privateKeyBase16)))
      case _ => conf.pure[Task]
    }

  private def checkHost(conf: NodeConf): Task[Unit] = {
    import coop.rchain.comm._
    conf.protocolServer.host.fold(Task.unit) { h =>
      Task
        .now(conf.protocolServer.allowPrivateAddresses)
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

  private def checkPorts(conf: NodeConf): Task[NodeConf] = {
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

    def checkRChainProtocolPort(configuration: NodeConf): Task[NodeConf] =
      isLocalPortAvailable(configuration.protocolServer.port).ifM(
        configuration.pure[Task],
        if (configuration.protocolServer.useRandomPorts)
          for {
            port <- getFreePort
            _    <- log.info(s"Using random port $port as RChain Protocol port")
          } yield configuration.copy(
            protocolServer = configuration.protocolServer.copy(port = port)
          )
        else
          log.error(
            "Hint: Run me with --use-random-ports to use a random port for RChain Protocol port"
          ) >> Task.raiseError(new Exception("Invalid RChain Protocol port"))
      )

    def checkKademliaPort(configuration: NodeConf): Task[NodeConf] =
      isLocalPortAvailable(configuration.peersDiscovery.port).ifM(
        configuration.pure[Task],
        if (configuration.protocolServer.useRandomPorts)
          for {
            port <- getFreePort
            _    <- log.info(s"Using random port $port as Kademlia port")
          } yield configuration.copy(
            peersDiscovery = configuration.peersDiscovery.copy(port = port)
          )
        else
          log.error(
            "Hint: Run me with --use-random-ports to use a random port for Kademlia port"
          ) >> Task.raiseError(new Exception("Invalid Kademlia port"))
      )

    for {
      portsAvailability <- List(
                            ("http", conf.apiServer.portHttp),
                            ("admin http", conf.apiServer.portAdminHttp),
                            ("grpc server external", conf.apiServer.portGrpcExternal),
                            ("grpc server internal", conf.apiServer.portGrpcInternal)
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

  private def logConfiguration(
      conf: NodeConf,
      profile: Profile,
      configFile: Option[File],
      options: commandline.Options
  ): Task[Unit] =
    Task
      .sequence(
        Seq(
          log.info(s"Starting with profile ${profile.name}"),
          if (configFile.isEmpty)
            log.warn("No configuration file found, using defaults")
          else
            log.info(s"Using configuration file: ${configFile.get.getAbsolutePath}"),
          log.info(s"Running on network: ${conf.protocolServer.networkId}")
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
