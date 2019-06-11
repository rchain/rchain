package coop.rchain.node.configuration

import java.nio.file.{Path, Paths}

import collection.JavaConverters._
import scala.util.Try
import coop.rchain.blockstorage.{BlockDagFileStorage, FileLMDBIndexBlockStore}
import coop.rchain.casper.CasperConf
import coop.rchain.node.configuration.commandline.ConfigMapper
import com.typesafe.config._
import coop.rchain.crypto.PublicKey
import coop.rchain.rholang.interpreter.util.codec.Base58
import monix.eval.Task
import org.rogach.scallop.ScallopOption

object Configuration {

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

  def build(arguments: Seq[String]): Configuration = {
    val configurationE = Try(buildConfiguration(arguments)).toEither

    configurationE match {
      case Left(t) =>
        System.err.println(s"Can't build the configuration: ${t.getMessage}")
        System.exit(1)
      case _ =>
    }

    configurationE.right.get
  }

  private def loadConfigFile(configFile: Path): Config = {
    val configE = Try(ConfigFactory.parseFile(configFile.toFile)).toEither

    configE match {
      case Left(t) =>
        System.err.println(s"Can't parse the configuration file: ${t.getMessage}")
        System.exit(1)
      case _ =>
    }

    configE.right.get
  }

  private def buildConfiguration(arguments: Seq[String]): Configuration = {
    val options = commandline.Options(arguments)
    val profile = options.profile.toOption.flatMap(profiles.get).getOrElse(defaultProfile)
    val command = subcommand(options)

    val dataDir =
      if (command == Run) options.run.dataDir.getOrElse(profile.dataDir._1())
      else profile.dataDir._1()

    val baseConfig =
      ConfigFactory.parseMap(
        Map(DataDirConfigKey -> dataDir.toString).asJava,
        "base configuration"
      )

    val configFile    = options.configFile.getOrElse(dataDir.resolve("rnode.conf"))
    val optionsConfig = ConfigMapper.fromOptions(options)
    val fileConfig    = if (command == Run) loadConfigFile(configFile) else ConfigFactory.empty()

    val config = optionsConfig
      .withFallback(fileConfig)
      .withFallback(baseConfig)
      .withFallback(ConfigFactory.load())

    val configuration =
      createConfiguration(
        addDynamicParts(config),
        config.hasPath(CertificateConfigKey),
        config.hasPath(KeyConfigKey),
        command,
        profile.name,
        configFile
      )

    configuration.copy(printHelpToConsole = () => options.printHelp())
  }

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
      command: Command,
      profile: String,
      configFile: Path
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
      dataDir.resolve("casper-block-dag-file-storage-invalid-blocks-log"),
      dataDir.resolve("casper-block-dag-file-storage-invalid-blocks-crc"),
      dataDir.resolve("casper-block-dag-file-storage-checkpoints"),
      dataDir.resolve("casper-block-dag-file-storage-block-number-index"),
      server.dagStorageSize
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
    val grpcMaxMessageSize: Int =
      Math.max(
        MaxMessageSizeMinimumValue,
        Math.min(
          MaxMessageSizeMaximumValue,
          grpcServer.maxMessageSize
        )
      )

    new Configuration(
      config,
      command,
      profile,
      configFile,
      server.copy(maxMessageSize = maxMessageSize),
      grpcServer.copy(maxMessageSize = grpcMaxMessageSize),
      tls,
      casper.copy(createGenesis = server.standalone),
      blockStorage,
      blockDagStorage,
      kamon
    )
  }

  private def subcommand(options: commandline.Options): Command =
    options.subcommand match {
      case Some(options.eval)   => Eval(options.eval.fileNames())
      case Some(options.repl)   => Repl
      case Some(options.deploy) =>
        //TODO: change the defaults before main net
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
      case Some(options.propose)    => Propose
      case Some(options.showBlock)  => ShowBlock(options.showBlock.hash())
      case Some(options.showBlocks) =>
        import options.showBlocks._
        ShowBlocks(depth.getOrElse(1))
      case Some(options.visualizeBlocks) =>
        import options.visualizeBlocks._
        VisualizeDag(depth.getOrElse(-1), showJustificationLines.getOrElse(false))
      case Some(options.machineVerifiableDag) => MachineVerifiableDag
      case Some(options.run)                  => Run
      case Some(options.keygen) =>
        Keygen(options.keygen.algorithm(), options.keygen.privateKeyPath())
      case Some(options.lastFinalizedBlock) => LastFinalizedBlock
      case Some(options.dataAtName)         => DataAtName(options.dataAtName.name())
      case Some(options.contAtName)         => ContAtName(options.contAtName.name())
      case Some(options.bondingDeployGen) =>
        import options.bondingDeployGen._
        BondingDeployGen(bondKey(), ethAddr(), amount(), privateKey(), publicKey())
      case _ => Help
    }
}

final case class Configuration(
    underlying: Config,
    command: Command,
    profile: String,
    configurationFile: Path,
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
