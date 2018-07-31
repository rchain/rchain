package coop.rchain.node.configuration

import java.nio.file.{Path, Paths}

import coop.rchain.blockstorage.LMDBBlockStore
import coop.rchain.casper.CasperConf
import coop.rchain.comm.PeerNode
import coop.rchain.node.configuration.toml.{Configuration => TomlConfiguration}

object NodeConfiguration {

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

  private val DefaultBootstrapServer: PeerNode = PeerNode
    .parse("rnode://acd0b05a971c243817a0cfd469f5d1a238c60294@52.119.8.109:40400")
    .right
    .get

  def apply(arguments: Seq[String]): Configuration = {
    val options = commandline.Options(arguments)
    val profile = options.profile.toOption.flatMap(profiles.get).getOrElse(defaultProfile)
    println(s"Starting with profile ${profile.name}")
    val dataDir    = options.run.data_dir.getOrElse(profile.dataDir._1())
    val configFile = options.configFile.getOrElse(dataDir.resolve("rnode.toml")).toFile
    println(s"Using configuration file: $configFile")
    val config = toml.TomlConfiguration.from(configFile) match {
      case Left(error) =>
        println(s"Can't load the configuration file: $error")
        None
      case Right(c) => Some(c)
    }

    val effectiveDataDir =
      if (options.run.data_dir.isDefined) dataDir
      else config.flatMap(_.server.flatMap(_.dataDir)).getOrElse(dataDir)

    apply(effectiveDataDir, options, config)
  }

  private def apply(dataDir: Path,
                    options: commandline.Options,
                    config: Option[TomlConfiguration]): Configuration = {
    val command: Command = options.subcommand match {
      case Some(options.eval)        => Eval(options.eval.fileNames())
      case Some(options.repl)        => Repl
      case Some(options.diagnostics) => Diagnostics
      case Some(options.deploy)      => Deploy(options.deploy.location())
      case Some(options.deployDemo)  => DeployDemo
      case Some(options.propose)     => Propose
      case Some(options.showBlock)   => ShowBlock(options.showBlock.hash())
      case Some(options.showBlocks)  => ShowBlocks
      case Some(options.run)         => Run
      case _                         => Help
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
    val httpPort: Int = get(_.run.httpPort, _ => None, DefaultHttPort)
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
      mapSize
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

}

case class Profile(name: String, dataDir: (() => Path, String))
