package coop.rchain.node.configuration

import java.net.InetAddress
import java.nio.file.{Path, Paths}

import coop.rchain.blockstorage.LMDBBlockStore
import coop.rchain.casper.CasperConf
import coop.rchain.comm.{PeerNode, UPnP}
import coop.rchain.node.IpChecker
import coop.rchain.node.configuration.toml.{Configuration => TomlConfiguration}
import coop.rchain.shared.{Log, LogSource}

import monix.eval.Task

object Configuration {
  private implicit val logSource: LogSource = LogSource(this.getClass)

  private val dockerProfile =
    Profile("docker", dataDir = (() => Paths.get("/var/lib/rnode"), "Defaults to /var/lib/rnode"))

  private val defaultProfile =
    Profile("default",
            dataDir =
              (() => Paths.get(sys.props("user.home"), ".rnode"), "Defaults to $HOME/.rnode"))

  private val profiles: Map[String, Profile] =
    Map(defaultProfile.name -> defaultProfile, dockerProfile.name -> dockerProfile)

  private val DefaultPort                       = 40400
  private val DefaultGrpcPort                   = 40401
  private val DefaultHttPort                    = 40402
  private val DefaultMetricsPort                = 40403
  private val DefaultGrpcHost                   = "localhost"
  private val DefaultNoUpNP                     = false
  private val DefaultStandalone                 = false
  private val DefaultTimeout                    = 1000
  private val DefaultMapSize: Long              = 1024L * 1024L * 1024L
  private val DefaultCasperBlockStoreSize: Long = 1024L * 1024L * 1024L
  private val DefaultNumValidators              = 5
  private val DefaultValidatorSigAlgorithm      = "ed25519"
  private val DefaultCertificateFileName        = "node.certificate.pem"
  private val DefaultKeyFileName                = "node.key.pem"
  private val DefaultMaxNumOfConnections        = 500
  private val DefaultBootstrapServer: PeerNode = PeerNode
    .parse("rnode://de6eed5d00cf080fc587eeb412cb31a75fd10358@52.119.8.109:40400")
    .right
    .get

  def apply(arguments: Seq[String])(implicit log: Log[Task]): Task[Configuration] =
    for {
      options <- Task.delay(commandline.Options(arguments))
      profile <- Task.pure(options.profile.toOption.flatMap(profiles.get).getOrElse(defaultProfile))
      _       <- log.info(s"Starting with profile ${profile.name}")
      result  <- apply(options, subcommand(options), profile)
    } yield result

  private def apply(options: commandline.Options, command: Command, profile: Profile)(
      implicit log: Log[Task]): Task[Configuration] =
    if (command == Run) {
      for {
        dataDir    <- Task.pure(options.run.data_dir.getOrElse(profile.dataDir._1()))
        configFile <- Task.delay(options.configFile.getOrElse(dataDir.resolve("rnode.toml")).toFile)
        _          <- log.info(s"Using configuration file: $configFile")
        configE    <- Task.delay(toml.TomlConfiguration.from(configFile))
        _ <- configE match {
              case Right(_) => Task.unit
              case Left(e)  => log.warn(s"Can't load the configuration file: $e")
            }
        config <- Task.pure(configE.toOption)
        effectiveDataDir <- Task.pure(
                             if (options.run.data_dir.isDefined) dataDir
                             else config.flatMap(_.server.flatMap(_.dataDir)).getOrElse(dataDir))
        result <- Task.pure(apply(effectiveDataDir, options, config))
      } yield result
    } else {
      val dataDir = profile.dataDir._1()
      Task.pure(
        new Configuration(
          command,
          Server(
            None,
            DefaultPort,
            DefaultHttPort,
            DefaultMetricsPort,
            DefaultNoUpNP,
            DefaultTimeout,
            DefaultBootstrapServer,
            DefaultStandalone,
            dataDir,
            DefaultMapSize,
            DefaultMaxNumOfConnections
          ),
          GrpcServer(
            options.grpcHost.getOrElse(DefaultGrpcHost),
            options.grpcPort.getOrElse(DefaultGrpcPort)
          ),
          Tls(
            dataDir.resolve(DefaultCertificateFileName),
            Paths.get(DefaultKeyFileName),
            customCertificateLocation = false,
            customKeyLocation = false
          ),
          CasperConf(
            None,
            None,
            DefaultValidatorSigAlgorithm,
            None,
            None,
            DefaultNumValidators,
            dataDir.resolve("genesis"),
            None,
            createGenesis = false
          ),
          LMDBBlockStore.Config(dataDir.resolve("casper-block-store"), DefaultCasperBlockStoreSize),
          options
        )
      )
    }

  private def apply(dataDir: Path,
                    options: commandline.Options,
                    config: Option[TomlConfiguration]): Configuration = {
    val command: Command = options.subcommand match {
      case Some(options.eval)        => Eval(options.eval.fileNames())
      case Some(options.repl)        => Repl
      case Some(options.diagnostics) => Diagnostics
      case Some(options.deploy) =>
        import options.deploy._
        Deploy(from(), phloLimit(), phloPrice(), nonce(), location())
      case Some(options.deployDemo) => DeployDemo
      case Some(options.propose)    => Propose
      case Some(options.showBlock)  => ShowBlock(options.showBlock.hash())
      case Some(options.showBlocks) => ShowBlocks
      case Some(options.run)        => Run
      case _                        => Help
    }

    import commandline.Options._

    def getOpt[A](fo: commandline.Options => Option[A],
                  fc: TomlConfiguration => Option[A]): Option[A] =
      fo(options).orElse(config.flatMap(fc))

    def get[A](fo: commandline.Options => Option[A],
               fc: TomlConfiguration => Option[A],
               default: => A): A =
      getOpt(fo, fc).getOrElse(default)

    // gRPC
    val grpcHost: String = get(_.grpcHost, _.grpcServer.flatMap(_.host), DefaultGrpcHost)
    val grpcPort: Int    = get(_.grpcPort, _.grpcServer.flatMap(_.port), DefaultGrpcPort)

    // Server
    val port: Int     = get(_.run.port, _.server.flatMap(_.port), DefaultPort)
    val httpPort: Int = get(_.run.httpPort, _.server.flatMap(_.httpPort), DefaultHttPort)
    val metricsPort: Int =
      get(_.run.metricsPort, _.server.flatMap(_.metricsPort), DefaultMetricsPort)
    val noUpnp: Boolean = get(_.run.noUpnp, _.server.flatMap(_.noUpnp), DefaultNoUpNP)
    val defaultTimeout: Int =
      get(_.run.defaultTimeout, _.server.flatMap(_.defaultTimeout), DefaultTimeout)
    val bootstrap: PeerNode =
      get(_.run.bootstrap, _.server.flatMap(_.bootstrap), DefaultBootstrapServer)
    val standalone: Boolean =
      get(_.run.standalone, _.server.flatMap(_.standalone), DefaultStandalone)
    val host: Option[String] = getOpt(_.run.host, _.server.flatMap(_.host))
    val mapSize: Long        = get(_.run.map_size, _.server.flatMap(_.mapSize), DefaultMapSize)
    val casperBlockStoreSize: Long = get(_.run.casperBlockStoreSize,
                                         _.server.flatMap(_.casperBlockStoreSize),
                                         DefaultCasperBlockStoreSize)

    // TLS
    val certificate: Option[Path] = getOpt(_.run.certificate, _.tls.flatMap(_.certificate))
    val key: Option[Path]         = getOpt(_.run.key, _.tls.flatMap(_.key))

    val certificatePath: Path = certificate.getOrElse(dataDir.resolve(DefaultCertificateFileName))

    val keyPath: Path = key.getOrElse(dataDir.resolve(DefaultKeyFileName))

    // Validators
    val numValidators =
      get(_.run.numValidators, _.validators.flatMap(_.count), DefaultNumValidators)
    val bondsFile           = getOpt(_.run.bondsFile, _.validators.flatMap(_.bondsFile))
    val knownValidators     = getOpt(_.run.knownValidators, _.validators.flatMap(_.known))
    val validatorPublicKey  = getOpt(_.run.validatorPublicKey, _.validators.flatMap(_.publicKey))
    val validatorPrivateKey = getOpt(_.run.validatorPrivateKey, _.validators.flatMap(_.privateKey))
    val validatorSigAlgorithm = get(_.run.validatorSigAlgorithm,
                                    _.validators.flatMap(_.sigAlgorithm),
                                    DefaultValidatorSigAlgorithm)
    val walletsFile: Option[String] = getOpt(_.run.walletsFile, _.validators.flatMap(_.walletsFile))
    val maxNumOfConnections = get(_.run.maxNumOfConnections,
                                  _.server.flatMap(_.maxNumOfConnections),
                                  DefaultMaxNumOfConnections)

    val server = Server(
      host,
      port,
      httpPort,
      metricsPort,
      noUpnp,
      defaultTimeout,
      bootstrap,
      standalone,
      dataDir,
      mapSize,
      maxNumOfConnections
    )
    val grpcServer = GrpcServer(
      grpcHost,
      grpcPort
    )
    val tls = Tls(
      certificatePath,
      keyPath,
      certificate.isDefined,
      key.isDefined
    )

    val casper =
      CasperConf(
        validatorPublicKey,
        validatorPrivateKey,
        validatorSigAlgorithm,
        bondsFile,
        knownValidators,
        numValidators,
        dataDir.resolve("genesis"),
        walletsFile,
        standalone
      )
    val blockstorage = LMDBBlockStore.Config(
      dataDir.resolve("casper-block-store"),
      casperBlockStoreSize
    )

    new Configuration(
      command,
      server,
      grpcServer,
      tls,
      casper,
      blockstorage,
      options
    )
  }

  private def subcommand(options: commandline.Options): Command =
    options.subcommand match {
      case Some(options.eval)        => Eval(options.eval.fileNames())
      case Some(options.repl)        => Repl
      case Some(options.diagnostics) => Diagnostics
      case Some(options.deploy) =>
        import options.deploy._
        Deploy(from(), phloLimit(), phloPrice(), nonce(), location())
      case Some(options.deployDemo) => DeployDemo
      case Some(options.propose)    => Propose
      case Some(options.showBlock)  => ShowBlock(options.showBlock.hash())
      case Some(options.showBlocks) => ShowBlocks
      case Some(options.run)        => Run
      case _                        => Help
    }
}

final class Configuration(
    val command: Command,
    val server: Server,
    val grpcServer: GrpcServer,
    val tls: Tls,
    val casper: CasperConf,
    val blockstorage: LMDBBlockStore.Config,
    private val options: commandline.Options
) {
  import coop.rchain.catscontrib.Capture._
  private implicit val logSource: LogSource = LogSource(this.getClass)

  def printHelp(): Task[Unit] = Task.delay(options.printHelp())

  def fetchHost(implicit log: Log[Task]): Task[String] =
    for {
      externalAddress <- retriveExternalAddress
      host            <- fetchHost(externalAddress)
    } yield host

  private def fetchHost(externalAddress: Option[String])(implicit log: Log[Task]): Task[String] =
    server.host match {
      case Some(h) => Task.pure(h)
      case None    => whoAmI(server.port, externalAddress)
    }

  private def retriveExternalAddress: Task[Option[String]] =
    Task.delay {
      if (server.noUpnp) None
      else UPnP.assurePortForwarding(Seq(server.port))
    }

  private def check(source: String, from: String): Task[(String, Option[String])] =
    IpChecker.checkFrom[Task](from).map((source, _))

  private def checkNext(prev: (String, Option[String]),
                        next: Task[(String, Option[String])]): Task[(String, Option[String])] =
    prev._2.fold(next)(_ => Task.pure(prev))

  private def upnpIpCheck(externalAddress: Option[String]): Task[(String, Option[String])] =
    Task.delay(("UPnP", externalAddress.map(InetAddress.getByName(_).getHostAddress)))

  private def checkAll(externalAddress: Option[String]): Task[(String, String)] =
    for {
      r1 <- check("AmazonAWS service", "http://checkip.amazonaws.com")
      r2 <- checkNext(r1, check("WhatIsMyIP service", "http://bot.whatismyipaddress.com"))
      r3 <- checkNext(r2, upnpIpCheck(externalAddress))
      r4 <- checkNext(r3, Task.pure("failed to guess", Some("localhost")))
    } yield {
      val (s, Some(a)) = r4
      (s, a)
    }

  private def whoAmI(port: Int, externalAddress: Option[String])(
      implicit log: Log[Task]): Task[String] =
    for {
      _      <- log.info("flag --host was not provided, guessing your external IP address")
      r      <- checkAll(externalAddress)
      (s, a) = r
      _      <- log.info(s"guessed $a from source: $s")
    } yield a
}

case class Profile(name: String, dataDir: (() => Path, String))
