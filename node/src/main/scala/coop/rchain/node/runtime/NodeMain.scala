package coop.rchain.node.runtime

import cats.Parallel
import cats.effect.{Async, Resource, Sync, Temporal}
import cats.syntax.all._
import coop.rchain.casper.protocol.client.{DeployRuntime, GrpcDeployService, GrpcProposeService}
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.{Secp256k1, SignaturesAlg}
import coop.rchain.crypto.util.KeyUtil
import coop.rchain.models.syntax._
import coop.rchain.node.configuration.Configuration.Profile
import coop.rchain.node.configuration._
import coop.rchain.node.effects
import coop.rchain.node.effects.{ConsoleIO, GrpcReplClient}
import coop.rchain.node.web.VersionInfo
import coop.rchain.shared.StringOps.StringColors
import coop.rchain.shared.{Log, TerminalMode}
import org.slf4j.bridge.SLF4JBridgeHandler

import java.io.File
import java.nio.file.Path
import scala.jdk.CollectionConverters._
import scala.tools.jline.console.ConsoleReader
import scala.tools.jline.console.completer.StringsCompleter

object NodeMain {

  /**
    * Starts RNode instance
    *
    * @param options command line options
    */
  def startNode[F[_]: Parallel: Async: ConsoleIO: Log](
      options: commandline.Options
  ): F[Unit] = Sync[F].defer {
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
      // Update ports in node configuration
      // TODO: split this in two functions to make it more clear what's updated
      nodeConfUpdPorts <- checkPorts[F](nodeConf)

      // Update validator's private key if found in PEM file
      pemFilePathOpt = nodeConfUpdPorts.casper.validatorPrivateKeyPath
      privateKeyOpt  <- pemFilePathOpt.traverse(loadPrivateKeyFromPemFile[F](_))
      nodeConfUpdPem = privateKeyOpt
        .map { privKey =>
          // If key file is supplied by the user, it overrides plain text private key
          val casperConf =
            nodeConfUpdPorts.casper.copy(validatorPrivateKey = privKey.bytes.toHexString.some)
          nodeConfUpdPorts.copy(casper = casperConf)
        }
        .getOrElse(nodeConfUpdPorts)

      _ <- Log[F].info(VersionInfo.get)
      _ <- logConfiguration[F](nodeConfUpdPem, profile, configFile)

      // TODO: This check may be removed in the future after updating clients like a VSCode extension
      _ <- Log[F]
            .warn(
              "allow-private-addresses option is deprecated and will be removed in future releases."
            )
            .whenA(options.run.allowPrivateAddresses.isSupplied)

      _ <- checkShardNameOnlyAscii(nodeConf.casper.shardName)

      // Create node runtime
      _ <- NodeRuntime.start[F](nodeConfUpdPem, kamonConf)
    } yield ()
  }

  /**
    * Executes CLI commands
    *
    * @param options command line options
    * @param console console
    */
  def runCLI[F[_]: Async: ConsoleIO](
      options: commandline.Options
  ): F[Unit] = {
    val grpcPort =
      if (options.grpcPort.isSupplied) options.grpcPort() else commandline.Options.GrpcInternalPort

    // Clients for executing gRPC calls on remote RNode instance
    implicit val replServiceClient: GrpcReplClient[F] =
      new GrpcReplClient[F](
        options.grpcHost(),
        grpcPort,
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
        grpcPort,
        options.grpcMaxRecvMessageSize()
      )

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
          location,
          shardId
          ) =>
        val getPrivateKey =
          maybePrivateKey
            .map(_.pure[F])
            .orElse(maybePrivateKeyPath.map(decryptKeyFromPemFile[F]))
            .sequence
        for {
          privateKey <- getPrivateKey
          _ <- privateKey.fold(Sync[F].raiseError[Unit](new Exception("Private key is missing")))(
                DeployRuntime.deployFileProgram[F](
                  phlo,
                  phloPrice,
                  validAfterBlock,
                  _,
                  location,
                  shardId
                )
              )
        } yield ()
      case DeployStatus(deployId)       => DeployRuntime.deployStatus[F](deployId)
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
      case Status                => DeployRuntime.status[F]
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
          location(),
          shardId()
        )
      case Some(options.deployStatus) => DeployStatus(options.deployStatus.deploySignature())
      case Some(options.findDeploy)   => FindDeploy(options.findDeploy.deployId())
      case Some(options.propose)      => Propose(options.propose.printUnmatchedSends())
      case Some(options.showBlock)    => ShowBlock(options.showBlock.hash())
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
      case Some(options.status)               => Status
      case _                                  => Help
    }

  private def decryptKeyFromPemFile[F[_]: Sync: ConsoleIO](
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
        if (password.nonEmpty) password.pure[F] else requestForPassword[F]
      case None => requestForPassword[F]
    }

  def requestForPassword[F[_]: ConsoleIO]: F[String] =
    ConsoleIO[F].readPassword(
      "Variable RNODE_VALIDATOR_PASSWORD is not set, please enter password for keyfile. \n" +
        "Password for keyfile: "
    )

  /**
    * Loads validator key from PEM file.
    *
    * @param keyFilePath file path to PEM file with validator's private key
    * @return base 16 encoded private key
    */
  private def loadPrivateKeyFromPemFile[F[_]: Sync: ConsoleIO](keyFilePath: Path): F[PrivateKey] =
    decryptKeyFromPemFile[F](keyFilePath)

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

    for {
      portsAvailability <- List(
                            ("http", conf.apiServer.portHttp),
                            ("admin http", conf.apiServer.portAdminHttp),
                            ("grpc server external", conf.apiServer.portGrpcExternal),
                            ("grpc server internal", conf.apiServer.portGrpcInternal)
                          ).traverse {
                            _.traverse(isLocalPortAvailable)
                          }

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

  private def checkShardNameOnlyAscii[F[_]: Sync: Log](shardName: String): F[Unit] =
    (Log[F].error("Shard name should contain only ASCII characters") >>
      Sync[F].raiseError(new RuntimeException("Invalid shard name")))
      .unlessA(shardName.isEmpty || shardName.onlyAscii)

  private def logConfiguration[F[_]: Sync: Log](
      conf: NodeConf,
      profile: Profile,
      configFile: Option[File]
  ): F[Unit] =
    Log[F].info(s"Starting with profile ${profile.name}") *>
      (if (configFile.isEmpty) Log[F].warn("No configuration file found, using defaults")
       else Log[F].info(s"Using configuration file: ${configFile.get.getAbsolutePath}")) *>
      Log[F].info(s"Running on network: ${conf.protocolServer.networkId}") *>
      Log[F].info(s"Running on shard name (id): ${conf.casper.shardName}")

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def consoleIO[F[_]: Sync]: ConsoleIO[F] = {
    val console = new ConsoleReader()
    console.setHistoryEnabled(true)
    if (TerminalMode.readMode)
      console.setPrompt("rholang $ ".green.colorize)
    console.addCompleter(new StringsCompleter(ReplRuntime.keywords.asJava))
    effects.consoleIO(console)
  }
}
