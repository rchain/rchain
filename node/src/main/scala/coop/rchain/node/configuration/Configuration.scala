package coop.rchain.node.configuration

import java.nio.file.{Path, Paths}

import com.typesafe.config.{Config, ConfigFactory}

import collection.JavaConverters._
import scala.util.Try
import coop.rchain.blockstorage.FileLMDBIndexBlockStore
import coop.rchain.blockstorage.dag.BlockDagFileStorage
import coop.rchain.comm.PeerNode
import coop.rchain.node.configuration.commandline.ConfigMapper
import coop.rchain.node.diagnostics.{BatchInfluxDBReporter, UdpInfluxDBReporter}
import kamon.Kamon
import kamon.system.SystemMetrics
import kamon.zipkin.ZipkinReporter
import monix.eval.Task
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
    * @return Configuration instance
    */
  def build(options: commandline.Options): (NodeConf, Config) = {
    val profile = options.profile.toOption.flatMap(profiles.get).getOrElse(defaultProfile)
    val dataDir = options.run.dataDir.getOrElse(profile.dataDir._1())
    val configFile = options.run.configFile
      .getOrElse(
        dataDir.resolve("rnode.conf")
      )
      .toFile

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
      if (configFile.exists())
        ConfigSource.file(configFile)
      else
        ConfigSource.empty
    val defaultConfig = ConfigSource
      .resources("defaults.conf")
      .withFallback(
        ConfigSource.string(
          s"default-data-dir = $dataDir"
        )
      )

    // Throw an error is unknown keys found
    //implicit val hint = ProductHint[NodeConf](allowUnknownKeys = false)
    // Custom reader for PeerNode type
    implicit val PeerNodeReader = ConfigReader.fromString[PeerNode](
      ConvertHelpers.catchReadError(s => PeerNode.fromAddress(s).getOrElse(null))
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
        System.err.println(s"Can't build the configuration: ${t}")
        System.exit(1)
      case _ =>
    }
    val nodeConf = nodeConfE.right.get

    val kamonConf = ConfigFactory.parseString(mergedConf.at("kamon").value().right.get.render())

    (nodeConf, kamonConf)
  }

  final case class Profile(name: String, dataDir: (() => Path, String))

  private val dockerProfile =
    Profile(
      "docker",
      dataDir = (
        () => Paths.get("/var/lib/rnode"),
        "Defaults to /var/lib/rnode"
      )
    )

  private val defaultProfile =
    Profile(
      "default",
      dataDir = (
        () => Paths.get(sys.props("user.home"), ".rnode"),
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
}
