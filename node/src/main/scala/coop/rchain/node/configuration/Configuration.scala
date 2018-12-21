package coop.rchain.node.configuration

import java.io.File
import java.nio.file.{Path, Paths}

import cats.implicits._
import coop.rchain.blockstorage.{BlockDagFileStorage, FileLMDBIndexBlockStore}
import coop.rchain.casper.CasperConf
import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import coop.rchain.node.configuration.toml.error._
import coop.rchain.node.configuration.toml.{Configuration => TomlConfiguration}
import coop.rchain.shared.{Log, LogSource}
import coop.rchain.shared.StoreType
import coop.rchain.shared.StoreType._

import monix.eval.Task
import scala.concurrent.duration._

object Configuration {
  private implicit val logSource: LogSource = LogSource(this.getClass)

  private val dockerProfile =
    Profile("docker", dataDir = (() => Paths.get("/var/lib/rnode"), "Defaults to /var/lib/rnode"))

  private val defaultProfile =
    Profile(
      "default",
      dataDir = (() => Paths.get(sys.props("user.home"), ".rnode"), "Defaults to $HOME/.rnode")
    )

  private val profiles: Map[String, Profile] =
    Map(defaultProfile.name -> defaultProfile, dockerProfile.name -> dockerProfile)

  private val DefaultPort                       = 40400
  private val DefaultGrpcPortExternal           = 40401
  private val DefaultGrpcPortInternal           = 40402
  private val DefaultHttPort                    = 40403
  private val DefaultKademliaPort               = 40404
  private val DefaultGrpcHost                   = "localhost"
  private val DefaultDynamicHostAddress         = false
  private val DefaultNoUpNP                     = false
  private val DefaultStandalone                 = false
  private val DefaultTimeout                    = 2000
  private val DefaultGenesisValidator           = false
  private val DefaultMapSize: Long              = 1024L * 1024L * 1024L
  private val DefaultStoreType: StoreType       = LMDB
  private val DefaultCasperBlockStoreSize: Long = 1024L * 1024L * 1024L
  private val DefaultNumValidators              = 5
  private val DefaultValidatorSigAlgorithm      = "ed25519"
  private val DefaultCertificateFileName        = "node.certificate.pem"
  private val DefaultKeyFileName                = "node.key.pem"
  private val DefaultSecureRandomNonBlocking    = false
  private val DefaultMaxNumOfConnections        = 500
  private val DefaultRequiredSigns              = 0
  private val DefaultApprovalProtocolDuration   = 5.minutes
  private val DefaultApprovalProtocolInterval   = 5.seconds
  private val DefaultMaxMessageSize: Int        = 256 * 1024 // 0.25 MB
  // within range HTTP2 RFC 7540
  private val MaxMessageSizeMinimumValue: Int = DefaultMaxMessageSize
  private val MaxMessageSizeMaximumValue: Int = 10 * 1024 * 1024
  private val DefaultMinimumBond: Long        = 1L
  private val DefaultMaximumBond: Long        = Long.MaxValue
  private val DefaultHasFaucet: Boolean       = false

  private val DefaultBootstrapServer: PeerNode = PeerNode
    .fromAddress(
      "rnode://de6eed5d00cf080fc587eeb412cb31a75fd10358@52.119.8.109?protocol=40400&discovery=40404"
    )
    .right
    .get
  private val DefaultShardId = "rchain"

  private val DefaultKamonPrometheus  = false
  private val DefaultKamonInfluxDb    = false
  private val DefaultKamonZipkin      = false
  private val DefaultKamonSigar       = false
  private val DefaultInfluxDbHostname = "127.0.0.1"
  private val DefaultInfluxDbPort     = 8086
  private val DefaultInfluxDbDatabase = "rnode"
  private val DefaultInfluxDbProtocol = "http"

  private def loadConfigurationFile(
      configFile: File
  )(implicit log: Log[Task]): Task[Option[TomlConfiguration]] =
    for {
      _       <- log.info(s"Using configuration file: $configFile")
      configE <- Task.delay(toml.TomlConfiguration.from(configFile))
      exit <- configE.fold(
               {
                 case ConfigurationParseError(e) =>
                   Log[Task]
                     .error(s"Can't parse the configuration: $e")
                     .as(true)
                 case ConfigurationAstError(e) =>
                   Log[Task]
                     .error(s"The structure of the configuration is not valid: $e")
                     .as(true)
                 case ConfigurationFileNotFound(f) =>
                   Log[Task]
                     .warn(s"Configuration file $f not found")
                     .as(false)
               },
               kp(Task.now(false))
             )
      _      = if (exit) System.exit(1)
      config <- Task.pure(configE.toOption)
    } yield config

  def apply(arguments: Seq[String])(implicit log: Log[Task]): Task[Configuration] =
    for {
      options <- Task.delay(commandline.Options(arguments))
      profile <- Task.pure(options.profile.toOption.flatMap(profiles.get).getOrElse(defaultProfile))
      result  <- apply(options, subcommand(options), profile)
    } yield result

  private def apply(options: commandline.Options, command: Command, profile: Profile)(
      implicit log: Log[Task]
  ): Task[Configuration] =
    if (command == Run) {
      for {
        dataDir    <- Task.pure(options.run.dataDir.getOrElse(profile.dataDir._1()))
        _          = System.setProperty("rnode.data.dir", dataDir.toString)
        configFile <- Task.delay(options.configFile.getOrElse(dataDir.resolve("rnode.toml")).toFile)
        config     <- loadConfigurationFile(configFile)
        effectiveDataDir <- Task.pure(
                             if (options.run.dataDir.isDefined) dataDir
                             else config.flatMap(_.server.flatMap(_.dataDir)).getOrElse(dataDir)
                           )
        _      = System.setProperty("rnode.data.dir", effectiveDataDir.toString)
        result <- Task.pure(apply(effectiveDataDir, command, options, config))
        _      <- log.info(s"Starting with profile ${profile.name}")
      } yield result
    } else {
      val dataDir = profile.dataDir._1()
      System.setProperty("rnode.data.dir", dataDir.toString)
      Task.pure(
        new Configuration(
          command,
          Server(
            None,
            DefaultPort,
            DefaultHttPort,
            DefaultKademliaPort,
            DefaultDynamicHostAddress,
            DefaultNoUpNP,
            DefaultTimeout,
            DefaultBootstrapServer,
            DefaultStandalone,
            DefaultGenesisValidator,
            dataDir,
            DefaultMapSize,
            DefaultStoreType,
            DefaultMaxNumOfConnections,
            DefaultMaxMessageSize
          ),
          GrpcServer(
            options.grpcHost.getOrElse(DefaultGrpcHost),
            options.grpcPort.getOrElse(DefaultGrpcPortExternal),
            options.grpcPortInternal.getOrElse(DefaultGrpcPortInternal)
          ),
          Tls(
            dataDir.resolve(DefaultCertificateFileName),
            Paths.get(DefaultKeyFileName),
            customCertificateLocation = false,
            customKeyLocation = false,
            secureRandomNonBlocking = false
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
            createGenesis = false,
            shardId = DefaultShardId,
            approveGenesis = false,
            requiredSigs = -1,
            approveGenesisDuration = 100.days,
            approveGenesisInterval = 1.day,
            deployTimestamp = None,
            minimumBond = DefaultMinimumBond,
            maximumBond = DefaultMaximumBond,
            hasFaucet = DefaultHasFaucet
          ),
          FileLMDBIndexBlockStore.Config(
            dataDir.resolve("casper-block-store").resolve("storage"),
            dataDir.resolve("casper-block-store").resolve("index"),
            dataDir.resolve("casper-block-store").resolve("checkpoints"),
            DefaultCasperBlockStoreSize
          ),
          BlockDagFileStorage.Config(
            dataDir.resolve("casper-block-dag-file-storage-latest-messages-log"),
            dataDir.resolve("casper-block-dag-file-storage-latest-messages-crc"),
            dataDir.resolve("casper-block-dag-file-storage-block-metadata-log"),
            dataDir.resolve("casper-block-dag-file-storage-block-metadata-crc"),
            dataDir.resolve("casper-block-dag-file-storage-checkpoints")
          ),
          Kamon(prometheus = false, None, zipkin = false, sigar = false),
          options
        )
      )
    }

  private def apply(
      dataDir: Path,
      command: Command,
      options: commandline.Options,
      config: Option[TomlConfiguration]
  ): Configuration = {
    import commandline.Options._

    def getOpt[A](
        fo: commandline.Options => Option[A],
        fc: TomlConfiguration => Option[A]
    ): Option[A] =
      fo(options).orElse(config.flatMap(fc))

    def get[A](
        fo: commandline.Options => Option[A],
        fc: TomlConfiguration => Option[A],
        default: => A
    ): A =
      getOpt(fo, fc).getOrElse(default)

    // gRPC
    val grpcHost: String = get(_.grpcHost, _.grpcServer.flatMap(_.host), DefaultGrpcHost)
    val grpcPortExternal: Int =
      get(_.grpcPort, _.grpcServer.flatMap(_.port), DefaultGrpcPortExternal)
    val grpcPortInternal: Int =
      get(_.grpcPortInternal, _.grpcServer.flatMap(_.portInternal), DefaultGrpcPortInternal)

    // Server
    val port: Int     = get(_.run.port, _.server.flatMap(_.port), DefaultPort)
    val httpPort: Int = get(_.run.httpPort, _.server.flatMap(_.httpPort), DefaultHttPort)
    val kademliaPort: Int =
      get(_.run.kademliaPort, _.server.flatMap(_.kademliaPort), DefaultKademliaPort)
    val dynamicHostAddress: Boolean =
      get(
        _.run.dynamicHostAddress,
        _.server.flatMap(_.dynamicHostAddress),
        DefaultDynamicHostAddress
      )
    val noUpnp: Boolean = get(_.run.noUpnp, _.server.flatMap(_.noUpnp), DefaultNoUpNP)
    val defaultTimeout: Int =
      get(_.run.defaultTimeout, _.server.flatMap(_.defaultTimeout), DefaultTimeout)
    val bootstrap: PeerNode =
      get(_.run.bootstrap, _.server.flatMap(_.bootstrap), DefaultBootstrapServer)
    val standalone: Boolean =
      get(_.run.standalone, _.server.flatMap(_.standalone), DefaultStandalone)
    val genesisValidator: Boolean =
      get(_.run.genesisValidator, _.server.flatMap(_.genesisValidator), DefaultGenesisValidator)
    val requiredSigs =
      get(_.run.requiredSigs, _.validators.flatMap(_.requiredSigs), DefaultRequiredSigns)
    val genesisApproveInterval =
      get(
        _.run.interval,
        _.validators.flatMap(_.approveGenesisInterval),
        DefaultApprovalProtocolInterval
      )
    val genesisAppriveDuration =
      get(
        _.run.duration,
        _.validators.flatMap(_.approveGenesisDuration),
        DefaultApprovalProtocolDuration
      )

    val deployTimestamp = getOpt(_.run.deployTimestamp, _.validators.flatMap(_.deployTimestamp))

    val host: Option[String] = getOpt(_.run.host, _.server.flatMap(_.host))
    val mapSize: Long        = get(_.run.mapSize, _.server.flatMap(_.mapSize), DefaultMapSize)
    val storeType: StoreType =
      get(_.run.storeType, _.server.flatMap(_.storeType.flatMap(StoreType.from)), DefaultStoreType)
    val casperBlockStoreSize: Long = get(
      _.run.casperBlockStoreSize,
      _.server.flatMap(_.casperBlockStoreSize),
      DefaultCasperBlockStoreSize
    )

    // TLS
    val certificate: Option[Path] = getOpt(_.run.certificate, _.tls.flatMap(_.certificate))
    val key: Option[Path]         = getOpt(_.run.key, _.tls.flatMap(_.key))

    val certificatePath: Path = certificate.getOrElse(dataDir.resolve(DefaultCertificateFileName))

    val keyPath: Path = key.getOrElse(dataDir.resolve(DefaultKeyFileName))

    val secureRandomNonBlocking =
      get(_.run.secureRandomNonBlocking, _ => None, DefaultSecureRandomNonBlocking)

    // Validators
    val numValidators =
      get(_.run.numValidators, _.validators.flatMap(_.count), DefaultNumValidators)
    val bondsFile          = getOpt(_.run.bondsFile, _.validators.flatMap(_.bondsFile))
    val knownValidators    = getOpt(_.run.knownValidators, _.validators.flatMap(_.known))
    val validatorPublicKey = getOpt(_.run.validatorPublicKey, _.validators.flatMap(_.publicKey))

    val maybePrivateKeyPath =
      getOpt(
        _.run.validatorPrivateKeyPath.toOption.map(Right(_)),
        _.validators.flatMap(_.privateKeyPath).map(Right(_))
      )

    val maybePrivateKey =
      getOpt(_.run.validatorPrivateKey, _.validators.flatMap(_.privateKey)).map(Left(_))

    val validatorPrivateKey = maybePrivateKeyPath.orElse(maybePrivateKey)

    val validatorSigAlgorithm = get(
      _.run.validatorSigAlgorithm,
      _.validators.flatMap(_.sigAlgorithm),
      DefaultValidatorSigAlgorithm
    )
    val walletsFile: Option[String] = getOpt(_.run.walletsFile, _.validators.flatMap(_.walletsFile))
    val minimumBond =
      get(_.run.minimumBond, _.validators.flatMap(_.minimumBond), DefaultMinimumBond)
    val maximumBond =
      get(_.run.maximumBond, _.validators.flatMap(_.maximumBond), DefaultMaximumBond)
    val hasFaucet = get(_.run.hasFaucet, _.validators.flatMap(_.hasFaucet), DefaultHasFaucet)
    val maxNumOfConnections = get(
      _.run.maxNumOfConnections,
      _.server.flatMap(_.maxNumOfConnections),
      DefaultMaxNumOfConnections
    )

    val maxMessageSize: Int =
      Math.max(
        MaxMessageSizeMinimumValue,
        Math.min(
          MaxMessageSizeMaximumValue,
          get(_.run.maxMessageSize, _.server.flatMap(_.maxMessageSize), DefaultMaxMessageSize)
        )
      )

    val shardId = get(_.run.shardId, _.validators.flatMap(_.shardId), DefaultShardId)

    val kamonPrometheus =
      get(_.run.prometheus, _.kamon.flatMap(_.prometheus), DefaultKamonPrometheus)
    val kamonInfluxDb = get(kp(None), _.kamon.flatMap(_.influxDb), DefaultKamonInfluxDb)
    val kamonZipkin   = get(_.run.zipkin, _.kamon.flatMap(_.zipkin), DefaultKamonZipkin)
    val sigar         = get(_.run.sigar, _.kamon.flatMap(_.sigar), DefaultKamonSigar)

    val influxDb =
      if (kamonInfluxDb) {
        val influxDbHostname =
          get(kp(None), _.influxDb.flatMap(_.hostname), DefaultInfluxDbHostname)
        val influxDbPort = get(kp(None), _.influxDb.flatMap(_.port), DefaultInfluxDbPort)
        val influxDbDatabase =
          get(kp(None), _.influxDb.flatMap(_.database), DefaultInfluxDbDatabase)
        val influxDbProtocol =
          get(kp(None), _.influxDb.flatMap(_.protocol), DefaultInfluxDbProtocol)

        val influxDBAuth = get(kp(None), _.influxDb.map(_.authentication), None)
        val influxDBAuthentication =
          influxDBAuth.map(a => InfluxDBAuthentication(a.user, a.password))
        Some(
          InfluxDb(
            influxDbHostname,
            influxDbPort,
            influxDbDatabase,
            influxDbProtocol,
            influxDBAuthentication
          )
        )
      } else None

    val server = Server(
      host,
      port,
      httpPort,
      kademliaPort,
      host.isEmpty && dynamicHostAddress,
      noUpnp,
      defaultTimeout,
      bootstrap,
      standalone,
      genesisValidator,
      dataDir,
      mapSize,
      storeType,
      maxNumOfConnections,
      maxMessageSize
    )
    val grpcServer = GrpcServer(
      grpcHost,
      grpcPortExternal,
      grpcPortInternal
    )
    val tls = Tls(
      certificatePath,
      keyPath,
      certificate.isDefined,
      key.isDefined,
      secureRandomNonBlocking
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
        minimumBond,
        maximumBond,
        hasFaucet,
        requiredSigs,
        shardId,
        standalone,
        genesisValidator,
        genesisApproveInterval,
        genesisAppriveDuration,
        deployTimestamp
      )
    val blockstorage =
      FileLMDBIndexBlockStore.Config(
        dataDir.resolve("casper-block-store").resolve("storage"),
        dataDir.resolve("casper-block-store").resolve("index"),
        dataDir.resolve("casper-block-store").resolve("checkpoints"),
        casperBlockStoreSize
      )
    val blockDagStorage = BlockDagFileStorage.Config(
      dataDir.resolve("casper-block-dag-file-storage-latest-messages-log"),
      dataDir.resolve("casper-block-dag-file-storage-latest-messages-crc"),
      dataDir.resolve("casper-block-dag-file-storage-block-metadata-log"),
      dataDir.resolve("casper-block-dag-file-storage-block-metadata-crc"),
      dataDir.resolve("casper-block-dag-file-storage-checkpoints")
    )

    val kamon =
      Kamon(
        kamonPrometheus,
        influxDb,
        kamonZipkin,
        sigar
      )

    new Configuration(
      command,
      server,
      grpcServer,
      tls,
      casper,
      blockstorage,
      blockDagStorage,
      kamon,
      options
    )
  }

  private def subcommand(options: commandline.Options): Command =
    options.subcommand match {
      case Some(options.eval)        => Eval(options.eval.fileNames())
      case Some(options.repl)        => Repl
      case Some(options.diagnostics) => Diagnostics
      case Some(options.deploy)      =>
        //TODO: change the defaults before main net
        import options.deploy._
        Deploy(
          from.getOrElse("0x"),
          phloLimit(),
          phloPrice(),
          nonce.getOrElse(0),
          location()
        )
      case Some(options.deployDemo) => DeployDemo
      case Some(options.propose)    => Propose
      case Some(options.showBlock)  => ShowBlock(options.showBlock.hash())
      case Some(options.showBlocks) =>
        import options.showBlocks._
        ShowBlocks(depth.getOrElse(1))
      case Some(options.visualizeBlocks) =>
        import options.visualizeBlocks._
        VisualizeDag(depth.getOrElse(-1), showJustificationLines.getOrElse(false))
      case Some(options.run)        => Run
      case Some(options.dataAtName) => DataAtName(options.dataAtName.name())
      case Some(options.contAtName) => ContAtName(options.contAtName.name())
      case Some(options.bondingDeployGen) =>
        import options.bondingDeployGen._
        BondingDeployGen(bondKey(), ethAddr(), amount(), privateKey(), publicKey())
      case Some(options.faucetBondingDeployGen) =>
        import options.faucetBondingDeployGen._
        FaucetBondingDeployGen(amount(), sigAlgorithm(), privateKey(), publicKey())
      case _ => Help
    }
}

final class Configuration(
    val command: Command,
    val server: Server,
    val grpcServer: GrpcServer,
    val tls: Tls,
    val casper: CasperConf,
    val blockstorage: FileLMDBIndexBlockStore.Config,
    val blockDagStorage: BlockDagFileStorage.Config,
    val kamon: Kamon,
    private val options: commandline.Options
) {
  def printHelp(): Task[Unit] = Task.delay(options.printHelp())
}

case class Profile(name: String, dataDir: (() => Path, String))
