package coop.rchain.node.configuration

import java.io.File
import java.nio.file.{Path, Paths}

import com.typesafe.config.{Config, ConfigFactory}
import coop.rchain.comm.{CommError, PeerNode}
import coop.rchain.node.configuration.commandline.ConfigMapper
import pureconfig._
import pureconfig.generic.ProductHint
import pureconfig.generic.auto._

/**
  * Configuration for RChain node instance
  */
object Configuration {

  /**
    * Builds Configuration instance from CLI options.
    * If config file is provided as part of CLI options, it shall be parsed and merged
    * with CLI options having higher priority.
    * @param options CLI options
    * @return RNode configuration instance
    * @return Profile used to create RNode configuration
    * @return Config file used to create RNode configuration
    * @return Kamon configuration instance
    */
  def build(options: commandline.Options): (NodeConf, Profile, Option[File], Config) = {
    val profile = options.profile.toOption.flatMap(profiles.get).getOrElse(defaultProfile)
    val dataDir = options.run.dataDir.getOrElse(profile.dataDir._1)
    val configFilePath = options.run.configFile
      .getOrElse(
        dataDir.resolve("rnode.conf")
      )
    val configFile: Option[File] =
      if (configFilePath.toFile.exists())
        Some(configFilePath.toFile)
      else
        None

    /**
      * Target Configuration is a merge of 3 sources according to the following priority, highest to lowers
      * 1. optionsConfig - CLI options
      * 2. fileConfig - `.conf` file provided via CLI option `-c`
      * 3. defaultConfig - defaults in `node/src/main/resources/defaults.conf`,
      *    defaultConfig is modified with parameters set in profile
      */
    val optionsConfig = ConfigSource.fromConfig(
      ConfigMapper.fromOptions(options)
    )
    val fileConfig =
      if (configFile.nonEmpty)
        ConfigSource.file(configFile.get)
      else
        ConfigSource.empty
    val defaultConfig = ConfigSource
      .resources("defaults.conf")
      .withFallback(
        ConfigSource.string(
          s"default-data-dir = $dataDir"
        )
      )

    // Throw an error if unknown keys found
    implicit val hint = ProductHint[NodeConf](allowUnknownKeys = false)
    // Custom reader for PeerNode type
    def commErrToThrow(commErr: CommError) =
      new Exception(CommError.errorMessage(commErr))

    implicit val peerNodeReader = ConfigReader.fromStringTry[PeerNode](
      PeerNode.fromAddress(_).left.map(commErrToThrow).toTry
    )

    // Make Long values support size-in-bytes format, e.g. 16M
    implicit val myIntReader = ConfigReader.fromString[Long](
      ConvertHelpers.catchReadError(s => ConfigFactory.parseString(s"v = $s").getBytes("v"))
    )
    // Merge configs and create NodeConf instance
    val mergedConf = optionsConfig
      .withFallback(fileConfig)
      .withFallback(defaultConfig)
    val nodeConfE = mergedConf.load[NodeConf]

    nodeConfE match {
      case Left(t) =>
        System.err.println(
          s"Can't build the configuration: ${t.toList.foldLeft("") {
            _ + "\n" + _
          }}"
        )
        System.exit(1)
      case _ =>
    }
    val nodeConf = nodeConfE.right.get

    val kamonConfigFile = dataDir.resolve("kamon.conf").toFile
    val kamonDefaultConfig =
      if (kamonConfigFile.exists())
        ConfigFactory.parseFile(kamonConfigFile)
      else
        ConfigFactory.empty()
    val kamonConf = kamonDefaultConfig.withFallback(ConfigFactory.load("kamon.conf"))

    val nodeConf_ = checkDevMode(nodeConf)

    (nodeConf_, profile, configFile, kamonConf)

  }

  def checkDevMode(nodeConf: NodeConf): NodeConf =
    if (nodeConf.devMode) {
      nodeConf
    } else {
      if (nodeConf.dev.deployerPrivateKey.nonEmpty)
        System.out.println("Node is not in dev mode, ignoring --deployer-private-key")
      nodeConf.copy(dev = DevConf(deployerPrivateKey = None))
    }

  final case class Profile(name: String, dataDir: (Path, String))

  private val dockerProfile =
    Profile(
      "docker",
      dataDir = (
        Paths.get("/var/lib/rnode"),
        "Defaults to /var/lib/rnode"
      )
    )

  private val defaultProfile =
    Profile(
      "default",
      dataDir = (
        Paths.get(sys.props("user.home"), ".rnode"),
        "Defaults to $HOME/.rnode"
      )
    )

  def profiles: Map[String, Profile] =
    Map(
      defaultProfile.name -> defaultProfile,
      dockerProfile.name  -> dockerProfile
    )

  // within range HTTP2 RFC 7540
  // TODO do we need this constraint? Let user set any but warn?
  // private val MaxMessageSizeMinimumValue: Int = 256 * 1024 // 0.25 MB
  // private val MaxMessageSizeMaximumValue: Int = 10 * 1024 * 1024

  def defaultConf(dataDir: Path = Paths.get(".")) =
    ConfigSource
      .resources("defaults.conf")
      .withFallback(
        ConfigSource.string(
          s"default-data-dir = $dataDir"
        )
      )
}
