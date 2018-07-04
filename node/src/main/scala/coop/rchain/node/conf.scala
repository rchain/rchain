package coop.rchain.node

import java.net.{InetAddress, NetworkInterface}
import java.nio.file.{Path, Paths}

import com.typesafe.scalalogging.Logger
import coop.rchain.comm.UPnP
import coop.rchain.casper.CasperConf
import org.rogach.scallop._
import coop.rchain.catscontrib._, Catscontrib._, ski._
import scala.collection.JavaConverters._

// TODO replace with default config file when CORE-512 is resolved
case class Profile(name: String, dataDir: (() => Path, String))

object Profile {
  val docker =
    Profile("docker", dataDir = (() => Paths.get("/var/lib/rnode"), "Defaults to /var/lib/rnode"))
  val default =
    Profile("default",
            dataDir =
              (() => Paths.get(sys.props("user.home"), ".rnode"), "Defaults to $HOME/.rnode"))

  val profiles =
    Map(default.name -> default, docker.name -> docker)
}

final case class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  version(s"RChain Node ${BuildInfo.version}")

  val profile = opt[String](default = Some("default"), name = "profile")
    .map(Profile.profiles.getOrElse(_, Profile.default))

  val grpcPort =
    opt[Int](default = Some(50000), descr = "Port used for gRPC API.")

  val grpcHost =
    opt[String](default = Some("localhost"),
                descr = "Hostname or IP of node on which gRPC service is running.")

  val diagnostics = new Subcommand("diagnostics") {
    descr("Node diagnostics")
  }
  addSubcommand(diagnostics)

  val run = new Subcommand("run") {

    val noUpnp = opt[Boolean](default = Some(false), descr = "Use this flag to disable UpNp.")

    val defaultTimeout =
      opt[Int](default = Some(1000),
               descr = "Default timeout for roundtrip connections. Default 1 second.")

    val certificate =
      opt[Path](required = false,
                short = 'c',
                descr =
                  "Path to node's X.509 certificate file, that is being used for identification")

    val key =
      opt[Path](required = false,
                short = 'k',
                descr =
                  "Path to node's private key PEM file, that is being used for TLS communication")

    val port =
      opt[Int](default = Some(30304), short = 'p', descr = "Network port to use.")

    val httpPort =
      opt[Int](default = Some(8080),
               descr = "HTTP port (deprecated - all API features will be ported to gRPC API).")

    val metricsPort =
      opt[Int](default = Some(9095), descr = "Port used by metrics API.")

    val numValidators = opt[Int](default = Some(5), descr = "Number of validators at genesis.")
    val bondsFile = opt[String](
      default = None,
      descr = "Plain text file consisting of lines of the form `<pk> <stake>`, " +
        "which defines the bond amounts for each validator at genesis. " +
        "<pk> is the public key (in base-16 encoding) identifying the validator and <stake>" +
        "is the amount of Rev they have bonded (an integer). Note: this overrides the --num-validators option."
    )

    val bootstrap =
      opt[String](default =
                    Some("rnode://acd0b05a971c243817a0cfd469f5d1a238c60294@216.83.154.106:30304"),
                  short = 'b',
                  descr = "Bootstrap rnode address for initial seed.")

    val standalone = opt[Boolean](default = Some(false),
                                  short = 's',
                                  descr = "Start a stand-alone node (no bootstrapping).")

    val host = opt[String](default = None, descr = "Hostname or IP of this node.")

    val data_dir = opt[Path](required = false,
                             descr = "Path to data directory. Defaults to $HOME/.rnode",
                             default = profile.toOption.map(_.dataDir._1.apply()))

    val map_size = opt[Long](required = false,
                             descr = "Map size (in bytes)",
                             default = Some(1024L * 1024L * 1024L))

    val validatorPublicKey = opt[String](
      default = None,
      descr = "Base16 encoding of the public key to use for signing a proposed blocks. " +
        "Can be inferred from the private key for some signature algorithms."
    )

    val validatorPrivateKey = opt[String](
      default = None,
      descr = "Base16 encoding of the private key to use for signing a proposed blocks.")

    val validatorSigAlgorithm = opt[String](
      default = Some("ed25519"),
      descr = "Name of the algorithm to use for signing proposed blocks. " +
        "Currently supported values: ed25519")

    def certificatePath: Path =
      certificate.toOption
        .getOrElse(Paths.get(data_dir().toString, "node.certificate.pem"))

    def keyPath: Path =
      key.toOption
        .getOrElse(Paths.get(data_dir().toString, "node.key.pem"))

    def fetchHost(upnp: UPnP): String =
      host.toOption match {
        case Some(host) => host
        case None       => whoami(port(), upnp)
      }
  }
  addSubcommand(run)

  val repl = new Subcommand("repl") {
    descr("Starts a thin client, that will connect to existing node. See grpcHost and grpcPort.")
  }
  addSubcommand(repl)

  val eval = new Subcommand("eval") {
    descr(
      "Starts a thin client that will evaluate rholang in file on a existing running node. See grpcHost and grpcPort.")

    val fileNames = trailArg[List[String]](required = true)(stringListConverter)
  }
  addSubcommand(eval)

  val deployDemo = new Subcommand("deploy-demo") {
    descr(
      "Demo sending some placeholder Deploy operations to Casper on an existing running node at regular intervals")
  }
  addSubcommand(deployDemo)

  val deploy = new Subcommand("deploy") {
    descr(
      "Deploy a Rholang source file to Casper on an existing running node. " +
        "The deploy will be packaged and sent as a block to the network depending " +
        "on the configuration of the Casper instance.")
    val location = trailArg[String]()
  }
  addSubcommand(deploy)

  val showBlock = new Subcommand("show-block") {
    descr(
      "View properties of a block known by Casper on an existing running node." +
        "Output includes: parent hashes, storage contents of the tuplespace.")
    val hash =
      trailArg[String](name = "hash", required = true, descr = "the hash value of the block")
  }
  addSubcommand(showBlock)

  val showBlocks = new Subcommand("show-blocks") {
    descr(
      "View list of blocks on the main chain in the current Casper view on an existing running node.")
  }
  addSubcommand(showBlocks)

  val propose = new Subcommand("propose") {
    descr(
      "Force Casper (on an existing running node) to propose a block based on its accumulated deploys.")
  }
  addSubcommand(propose)

  private def check(source: String, from: String): PartialFunction[Unit, (String, String)] =
    Function.unlift(Unit => IpChecker.checkFrom(from).map(ip => (source, ip)))

  private def checkAll: (String, String) = {
    val func: PartialFunction[Unit, (String, String)] =
      check("AmazonAWS service", "http://checkip.amazonaws.com") orElse
        check("WhatIsMyIP service", "http://bot.whatismyipaddress.com") orElse {
        case _ => ("failed to guess", "localhost")
      }

    func.apply(())
  }

  private def whoami(port: Int, upnp: UPnP): String = {
    println("INFO - flag --host was not provided, guessing your external IP address")

    val (source, ip) = upnp.externalAddress
      .map(addy => ("uPnP", InetAddress.getByName(addy).getHostAddress))
      .getOrElse(checkAll)
    println(s"INFO - guessed $ip from source: $source")
    ip
  }

  def casperConf: CasperConf = CasperConf(
    run.validatorPublicKey.toOption,
    run.validatorPrivateKey.toOption,
    run.validatorSigAlgorithm(),
    run.bondsFile.toOption,
    run.numValidators(),
    run.data_dir().resolve("validators")
  )

  verify()
}
