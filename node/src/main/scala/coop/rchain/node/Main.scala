package coop.rchain.node

import java.io.File
import java.nio.file.Path
import cats.Parallel
import cats.effect._
import cats.syntax.all._
import coop.rchain.casper.util.comm._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.{Secp256k1, SignaturesAlg}
import coop.rchain.crypto.util.KeyUtil
import coop.rchain.monix.Monixable
import coop.rchain.node.benchmark.{ConcurrentReplayBench, LeaderfulHardening}
import coop.rchain.node.configuration.Configuration.Profile
import coop.rchain.node.configuration._
import coop.rchain.node.effects._
import coop.rchain.node.runtime.NodeRuntime
import coop.rchain.node.web.VersionInfo
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

    // Main scheduler for all CPU bounded tasks
    // Should always be passed as implicit dependency.
    // All other schedulers should be explicit.
    implicit val scheduler: Scheduler = Scheduler.computation(
      Math.max(java.lang.Runtime.getRuntime.availableProcessors, 2),
      "node-runner",
      reporter = UncaughtExceptionLogger
    )
    implicit val console: ConsoleIO[Task] = consoleIO
    implicit val log: Log[Task]           = effects.log
    implicit val eventLog: EventLog[Task] = EventLog.eventLogger

    // Ensure terminal is restored on exit
    sys.addShutdownHook {
      console.close.unsafeRunSync
    }

    // Parse CLI options
    val options = commandline.Options(args)
    if (options.subcommand.contains(options.run))
      // Start the node
      startNode[Task](options).unsafeRunSync
    else if (options.subcommand.contains(options.bench))
      startBench[Task](options).unsafeRunSync
    //or
    else
      // Execute CLI command
      runCLI[Task](options).unsafeRunSync
  }

  private def startBench[F[_]: Monixable: ConcurrentEffect: Parallel: ContextShift: Timer: ConsoleIO: Log: EventLog](
      options: commandline.Options
  )(implicit scheduler: Scheduler): F[Unit] = {
    implicit val time: Time[F] = effects.time
    if (options.subcommands.contains(options.bench.concurrency))
      ConcurrentReplayBench.go(options.bench.concurrency.maxConcurrentTx(), options.bench.dataDir())
    else if (options.subcommands.contains(options.bench.leaderful))
      LeaderfulHardening.run(
        options.bench.dataDir(),
        options.bench.leaderful.validatorsNum(),
        options.bench.leaderful.usersNum(),
        options.bench.leaderful.maxTxPerBlock(),
        options.bench.leaderful.epochLength()
      )
    else new Exception(s"Please select benchmark mode").raiseError
  }

  /**
    * Starts RNode instance
    * @param options command line options
    */
  private def startNode[F[_]: Monixable: ConcurrentEffect: Parallel: ContextShift: Timer: ConsoleIO: Log: EventLog](
      options: commandline.Options
  )(implicit s: Scheduler): F[Unit] = Sync[F].defer {
    // Create merged configuration from CLI options and config file
    val (nodeConf, profile, configFile, kamonConf) = Configuration.build(options)
    // This system variable is used in Logger config file `node/src/main/resources/logback.xml`
    val _ = System.setProperty("rnode.data.dir", nodeConf.storage.dataDir.toString)

    // and start node
    // TODO : Enable it earlier once we have JDK with https://bugs.openjdk.java.net/browse/JDK-8218960 fixed
    // https://www.slf4j.org/legacy.html#jul-to-slf4j
    SLF4JBridgeHandler.removeHandlersForRootLogger()
    SLF4JBridgeHandler.install()
    for {
      _               <- checkHost[F](nodeConf)
      confWithPorts   <- checkPorts[F](nodeConf)
      confWithDecrypt <- loadPrivateKeyFromFile[F](confWithPorts)
      _               <- Log[F].info(VersionInfo.get)
      _               <- logConfiguration[F](confWithDecrypt, profile, configFile)

      // Create node runtime
      _ <- NodeRuntime.start[F](confWithDecrypt, kamonConf)
    } yield ()
  }

  /**
    * Executes CLI commands
    * @param options command line options
    * @param console console
    */
  private def runCLI[F[_]: Sync: Monixable: ConsoleIO: Timer](
      options: commandline.Options
  ): F[Unit] = {
    // Clients for executing gRPC calls on remote RNode instance
    implicit val replServiceClient: GrpcReplClient[F] =
      new GrpcReplClient[F](
        options.grpcHost(),
        options.grpcPort(),
        options.grpcMaxRecvMessageSize()
      )
    implicit val deployServiceClient: GrpcDeployService[F] =
      new GrpcDeployService[F](
        options.grpcHost(),
        options.grpcPort(),
        options.grpcMaxRecvMessageSize()
      )
    implicit val proposeServiceClient: GrpcProposeService[F] =
      new GrpcProposeService[F](
        options.grpcHost(),
        options.grpcPort(),
        options.grpcMaxRecvMessageSize()
      )

    implicit val time: Time[F] = effects.time

    val program = subcommand(options) match {
      case Eval(files, printUnmatchedSendsOnly) =>
        new ReplRuntime().evalProgram[F](files, printUnmatchedSendsOnly)
      case Repl => new ReplRuntime().replProgram[F].as(())
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
            .map(_.pure[F])
            .orElse(maybePrivateKeyPath.map(decryptKeyFromCon[F]))
            .sequence
        for {
          privateKey <- getPrivateKey
          _ <- privateKey.fold(Sync[F].raiseError[Unit](new Exception("Private key is missing")))(
                DeployRuntime.deployFileProgram[F](
                  phlo,
                  phloPrice,
                  validAfterBlock,
                  _,
                  location
                )
              )
        } yield ()
      case FindDeploy(deployId)         => DeployRuntime.findDeploy[F](deployId)
      case Propose(printUnmatchedSends) => DeployRuntime.propose[F](printUnmatchedSends)
      case ShowBlock(hash)              => DeployRuntime.getBlock[F](hash)
      case ShowBlocks(depth)            => DeployRuntime.getBlocks[F](depth)
      case VisualizeDag(depth, showJustificationLines) =>
        DeployRuntime.visualizeDag[F](depth, showJustificationLines)
      case MachineVerifiableDag  => DeployRuntime.machineVerifiableDag[F]
      case DataAtName(name)      => DeployRuntime.listenForDataAtName[F](name)
      case ContAtName(names)     => DeployRuntime.listenForContinuationAtName[F](names)
      case Keygen(path)          => generateKey(path)
      case LastFinalizedBlock    => DeployRuntime.lastFinalizedBlock[F]
      case IsFinalized(hash)     => DeployRuntime.isFinalized[F](hash)
      case BondStatus(publicKey) => DeployRuntime.bondStatus[F](publicKey)
      case _                     => Sync[F].delay(options.printHelp())
    }

    Sync[F].delay(
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

  private def decryptKeyFromCon[F[_]: Sync: ConsoleIO](
      encryptedPrivateKeyPath: Path
  ): F[PrivateKey] =
    for {
      password   <- getValidatorPassword
      privateKey <- Secp256k1.parsePemFile[F](encryptedPrivateKeyPath, password)
    } yield privateKey

  private def generateKey[F[_]: Sync: ConsoleIO](path: Path): F[Unit] =
    for {
      password       <- ConsoleIO[F].readPassword("Enter password for keyfile: ")
      passwordRepeat <- ConsoleIO[F].readPassword("Repeat password: ")
      _ <- if (password != passwordRepeat) {
            ConsoleIO[F].println("Passwords do not match. Try again:") >> generateKey(path)
          } else if (password.isEmpty) {
            ConsoleIO[F].println("Password is empty. Try again:") >> generateKey(path)
          } else {
            for {
              sigAlgorithm <- SignaturesAlg(Secp256k1.name)
                               .map(_.pure[F])
                               .getOrElse(
                                 Sync[F]
                                   .raiseError(new IllegalStateException("Invalid algorithm name"))
                               )
              (sk, pk) = sigAlgorithm.newKeyPair

              privatePemKeyPath = path.resolve("rnode.key")
              publicPemKeyPath  = path.resolve("rnode.pub.pem")
              publicKeyHexFile  = path.resolve("rnode.pub.hex")
              _ <- KeyUtil.writeKeys[F](
                    sk,
                    pk,
                    sigAlgorithm,
                    password,
                    privatePemKeyPath,
                    publicPemKeyPath,
                    publicKeyHexFile
                  )
              _ <- ConsoleIO[F].println(
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
    */
  def getValidatorPassword[F[_]: Sync: ConsoleIO]: F[String] =
    sys.env.get(RNodeValidatorPasswordEnvVar) match {
      case Some(password) =>
        if (password.length > 0) password.pure[F] else requestForPassword[F]
      case None => requestForPassword[F]
    }

  def requestForPassword[F[_]: ConsoleIO]: F[String] =
    ConsoleIO[F].readPassword(
      "Variable RNODE_VALIDATOR_PASSWORD is not set, please enter password for keyfile. \n" +
        "Password for keyfile: "
    )

  /**
    * Loads validator key from file into configuration.
    * If key file is supplied by user, it overrides plain text private key
    * @param conf Node configuration instance
    * @return
    */
  private def loadPrivateKeyFromFile[F[_]: Sync: ConsoleIO](conf: NodeConf): F[NodeConf] =
    conf.casper.validatorPrivateKeyPath match {
      case Some(privateKeyPath) =>
        for {
          privateKeyBase16 <- decryptKeyFromCon[F](privateKeyPath)
                               .map(sk => Base16.encode(sk.bytes))
        } yield conf.copy(casper = conf.casper.copy(validatorPrivateKey = Some(privateKeyBase16)))
      case _ => conf.pure[F]
    }

  private def checkHost[F[_]: Sync: Log](conf: NodeConf): F[Unit] = {
    import coop.rchain.comm._
    conf.protocolServer.host.fold(().pure[F]) { h =>
      Sync[F]
        .delay(conf.protocolServer.allowPrivateAddresses)
        .ifM(isValidInetAddress[F](h), isValidPublicInetAddress[F](h))
        .ifM(
          ().pure[F],
          for {
            _ <- Log[F].error(
                  s"Kademlia hostname '$h' is not valid or it does not resolve to a public IP address"
                )
            _ <- Log[F].error("Hint: Run me with --allow-private-addresses in private networks")
            _ <- Sync[F].raiseError[Unit](new Exception("Invalid Kademlia hostname"))
          } yield ()
        )
    }
  }

  private def checkPorts[F[_]: Sync: Log](conf: NodeConf): F[NodeConf] = {
    def getFreePort: F[Int] =
      Resource
        .fromAutoCloseable(
          Sync[F].delay(new java.net.ServerSocket(0))
        )
        .use { socket =>
          Sync[F].delay {
            socket.setReuseAddress(true)
            socket.getLocalPort
          }
        }

    def isLocalPortAvailable(port: Int): F[Boolean] =
      Sync[F]
        .delay(new java.net.ServerSocket(port).close())
        .attempt
        .map(_.isRight)
        .ifM(
          true.pure[F],
          Log[F].error(s"Port $port is already in use!").as(false)
        )

    def checkRChainProtocolPort(configuration: NodeConf): F[NodeConf] =
      isLocalPortAvailable(configuration.protocolServer.port).ifM(
        configuration.pure[F],
        if (configuration.protocolServer.useRandomPorts)
          for {
            port <- getFreePort
            _    <- Log[F].info(s"Using random port $port as RChain Protocol port")
          } yield configuration.copy(
            protocolServer = configuration.protocolServer.copy(port = port)
          )
        else
          Log[F].error(
            "Hint: Run me with --use-random-ports to use a random port for RChain Protocol port"
          ) >> Sync[F].raiseError(new Exception("Invalid RChain Protocol port"))
      )

    def checkKademliaPort(configuration: NodeConf): F[NodeConf] =
      isLocalPortAvailable(configuration.peersDiscovery.port).ifM(
        configuration.pure[F],
        if (configuration.protocolServer.useRandomPorts)
          for {
            port <- getFreePort
            _    <- Log[F].info(s"Using random port $port as Kademlia port")
          } yield configuration.copy(
            peersDiscovery = configuration.peersDiscovery.copy(port = port)
          )
        else
          Log[F].error(
            "Hint: Run me with --use-random-ports to use a random port for Kademlia port"
          ) >> Sync[F].raiseError(new Exception("Invalid Kademlia port"))
      )

    import cats.instances.list._
    import cats.instances.tuple._

    for {
      portsAvailability <- List(
                            ("http", conf.apiServer.portHttp),
                            ("admin http", conf.apiServer.portAdminHttp),
                            ("grpc server external", conf.apiServer.portGrpcExternal),
                            ("grpc server internal", conf.apiServer.portGrpcInternal)
                          ).traverse { _.traverse(isLocalPortAvailable) }

      unavailablePorts = portsAvailability.filter(!_._2).map(_._1)

      _ <- if (unavailablePorts.isEmpty)
            ().pure[F]
          else
            Sync[F].raiseError(
              new Exception(
                s"Required ports are already in use: ${unavailablePorts.mkString(", ")}"
              )
            )

      rpPortConf       <- checkRChainProtocolPort(conf)
      kademliaPortConf <- checkKademliaPort(rpPortConf)
    } yield kademliaPortConf
  }

  private def logConfiguration[F[_]: Sync: Log](
      conf: NodeConf,
      profile: Profile,
      configFile: Option[File]
  ): F[Unit] =
    Log[F].info(s"Starting with profile ${profile.name}") *>
      (if (configFile.isEmpty) Log[F].warn("No configuration file found, using defaults")
       else Log[F].info(s"Using configuration file: ${configFile.get.getAbsolutePath}")) *>
      Log[F].info(s"Running on network: ${conf.protocolServer.networkId}")

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  private def consoleIO[F[_]: Sync]: ConsoleIO[F] = {
    val console = new ConsoleReader()
    console.setHistoryEnabled(true)
    if (TerminalMode.readMode)
      console.setPrompt("rholang $ ".green.colorize)
    console.addCompleter(new StringsCompleter(ReplRuntime.keywords.asJava))
    effects.consoleIO(console)
  }

}
