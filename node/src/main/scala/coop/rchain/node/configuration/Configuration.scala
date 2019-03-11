package coop.rchain.node.configuration

import java.nio.file.{Path, Paths}

import collection.JavaConverters._

import cats.implicits._

import coop.rchain.blockstorage.{BlockDagFileStorage, FileLMDBIndexBlockStore}
import coop.rchain.casper.CasperConf
import coop.rchain.catscontrib.ski._
import coop.rchain.node.configuration.commandline.ConfigMapper
import coop.rchain.shared.{Log, LogSource}

import com.typesafe.config._
import monix.eval.Task

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

  // within range HTTP2 RFC 7540
  private val MaxMessageSizeMinimumValue: Int = 256 * 1024 // 0.25 MB
  private val MaxMessageSizeMaximumValue: Int = 10 * 1024 * 1024

  private val DataDirConfigKey     = s"${hocon.Server.Key}.${hocon.Server.keys.DataDir}"
  private val CertificateConfigKey = s"${hocon.Tls.Key}.${hocon.Tls.keys.Certificate}"
  private val KeyConfigKey         = s"${hocon.Tls.Key}.${hocon.Tls.keys.Key}"
  private val GenesisPathConfigKey = s"${hocon.Casper.Key}.${hocon.Casper.keys.GenesisPath}"

  def build(arguments: Seq[String])(implicit log: Log[Task]): Task[Configuration] =
    for {
      config <- buildConfiguration(arguments).attempt
      exit <- config.fold(
               t => log.error(s"Can't build the configuration: ${t.getMessage}").as(true),
               kp(Task.now(false))
             )
      _ = if (exit) System.exit(1)
    } yield config.right.get

  private def buildConfiguration(
      arguments: Seq[String]
  )(implicit log: Log[Task]): Task[Configuration] =
    for {
      options       <- Task.delay(commandline.Options(arguments))
      profile       = options.profile.toOption.flatMap(profiles.get).getOrElse(defaultProfile)
      command       = subcommand(options)
      configRef     <- Task.delay(ConfigFactory.load())
      config        <- createConfig(options, command, profile)
      configuration = createConfiguration(config.withFallback(configRef), command)
      _             = System.setProperty("rnode.data.dir", configuration.server.dataDir.toString)
      _             <- if (command == Run) log.info(s"Starting with profile ${profile.name}") else Task.unit
    } yield configuration.copy(printHelpToConsole = () => options.printHelp())

  private def getDataDir(
      options: commandline.Options,
      command: Command,
      profile: Profile
  ): Path =
    if (command == Run) options.run.dataDir.getOrElse(profile.dataDir._1())
    else profile.dataDir._1()

  private def createConfig(options: commandline.Options, command: Command, profile: Profile)(
      implicit log: Log[Task]
  ): Task[Config] =
    for {
      dataDir        <- Task.delay(getDataDir(options, command, profile))
      baseConfig     = createBaseConfig(dataDir)
      externalConfig <- createExternalConfig(dataDir, options, command)
    } yield externalConfig.withFallback(baseConfig)

  private def createBaseConfig(dataDir: Path): Config =
    ConfigFactory.parseMap(
      Map(DataDirConfigKey -> dataDir.toString).asJava,
      "base configuration"
    )

  private def createExternalConfig(dataDir: Path, options: commandline.Options, command: Command)(
      implicit log: Log[Task]
  ): Task[Config] =
    createFileConfig(command, options.configFile.getOrElse(dataDir.resolve("rnode.conf")))
      .map(ConfigMapper.fromOptions(options).withFallback(_))

  private def createFileConfig(command: Command, configFile: Path)(
      implicit log: Log[Task]
  ): Task[Config] =
    if (command == Run) loadConfigurationFile(configFile)
    else ConfigFactory.empty().pure[Task]

  private def createConfiguration(
      config: Config,
      command: Command
  ): Configuration =
    createConfiguration(
      addDynamicParts(config),
      config.hasPath(CertificateConfigKey),
      config.hasPath(KeyConfigKey),
      command
    )

  private def loadConfigurationFile(configFile: Path)(implicit log: Log[Task]): Task[Config] =
    for {
      _       <- log.info(s"Trying to load configuration file: $configFile")
      configE <- Task.delay(ConfigFactory.parseFile(configFile.toFile)).attempt
      exit <- configE.fold(
               t => log.error(s"Can't parse the configuration file: ${t.getMessage}").as(true),
               kp(Task.now(false))
             )
      _      = if (exit) System.exit(1)
      config <- Task.pure(configE.fold(kp(ConfigFactory.empty()), identity))
    } yield config

  private def addDynamicParts(config: Config): Config = {
    val dataDir = Paths.get(config.getString(DataDirConfigKey))
    val map =
      Map(
        CertificateConfigKey -> dataDir.resolve("node.certificate.pem").toString,
        KeyConfigKey         -> dataDir.resolve("node.key.pem").toString,
        GenesisPathConfigKey -> dataDir.resolve("genesis").toString
      )
    config.withFallback(ConfigFactory.parseMap(map.asJava))
  }

  private def createConfiguration(
      config: Config,
      customCertificateLocation: Boolean,
      customKeyLocation: Boolean,
      command: Command
  ): Configuration = {
    val grpcServer = hocon.GrpcServer.fromConfig(config)
    val server     = hocon.Server.fromConfig(config)
    val tls = hocon.Tls
      .fromConfig(config)
      .copy(
        customCertificateLocation = customCertificateLocation,
        customKeyLocation = customKeyLocation
      )
    val kamon   = hocon.Kamon.fromConfig(config)
    val casper  = hocon.Casper.fromConfig(config)
    val dataDir = server.dataDir
    val blockDagStorage = BlockDagFileStorage.Config(
      dataDir.resolve("casper-block-dag-file-storage-latest-messages-log"),
      dataDir.resolve("casper-block-dag-file-storage-latest-messages-crc"),
      dataDir.resolve("casper-block-dag-file-storage-block-metadata-log"),
      dataDir.resolve("casper-block-dag-file-storage-block-metadata-crc"),
      dataDir.resolve("casper-block-dag-file-storage-equivocation-tracker-log"),
      dataDir.resolve("casper-block-dag-file-storage-equivocation-tracker-crc"),
      dataDir.resolve("casper-block-dag-file-storage-checkpoints")
    )
    val blockStorage =
      FileLMDBIndexBlockStore.Config(
        dataDir.resolve("casper-block-store").resolve("storage"),
        dataDir.resolve("casper-block-store").resolve("index"),
        dataDir.resolve("casper-block-store").resolve("approved-block"),
        dataDir.resolve("casper-block-store").resolve("checkpoints"),
        server.storeSize
      )
    val maxMessageSize: Int =
      Math.max(
        MaxMessageSizeMinimumValue,
        Math.min(
          MaxMessageSizeMaximumValue,
          server.maxMessageSize
        )
      )

    new Configuration(
      config,
      command,
      server.copy(maxMessageSize = maxMessageSize),
      grpcServer,
      tls,
      casper.copy(createGenesis = server.standalone),
      blockStorage,
      blockDagStorage,
      kamon
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

final case class Configuration(
    underlying: Config,
    command: Command,
    server: Server,
    grpcServer: GrpcServer,
    tls: Tls,
    casper: CasperConf,
    blockstorage: FileLMDBIndexBlockStore.Config,
    blockDagStorage: BlockDagFileStorage.Config,
    kamon: Kamon,
    printHelpToConsole: () => Unit = () => ()
) {
  def printHelp(): Task[Unit] = Task.delay(printHelpToConsole())
}

final case class Profile(name: String, dataDir: (() => Path, String))
