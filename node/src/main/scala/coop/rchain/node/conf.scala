package coop.rchain.node

import java.net.InetAddress
import java.nio.file.{Path, Paths}

import coop.rchain.casper.CasperConf
import coop.rchain.comm.PeerNode

import org.rogach.scallop._

// TODO replace with default config file when CORE-512 is resolved
case class Profile(name: String, dataDir: (() => Path, String))

object Converter {
  val bootstrapAddressConverter: ValueConverter[PeerNode] = new ValueConverter[PeerNode] {
    def parse(s: List[(String, List[String])]): Either[String, Option[PeerNode]] =
      s match {
        case (_, uri :: Nil) :: Nil =>
          PeerNode
            .parse(uri)
            .map(u => Right(Some(u)))
            .getOrElse(Left("can't parse the rnode bootstrap address"))
        case Nil => Right(None)
        case _   => Left("provide the rnode bootstrap address")
      }

    val argType: ArgType.V = ArgType.SINGLE
  }

}

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
  printedName = "rchain"

  val profile = opt[String](default = Some("default"),
                            name = "profile",
                            descr = "Which predefined set of defaults to use: default or docker.")
    .map(Profile.profiles.getOrElse(_, Profile.default))

  val grpcPort =
    opt[Int](default = Some(40401), descr = "Port used for gRPC API.")

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
      opt[Int](default = Some(40400), short = 'p', descr = "Network port to use.")

    val httpPort =
      opt[Int](default = Some(40402),
               descr = "HTTP port (deprecated - all API features will be ported to gRPC API).")

    val metricsPort =
      opt[Int](default = Some(40403), descr = "Port used by metrics API.")

    val numValidators = opt[Int](default = Some(5), descr = "Number of validators at genesis.")
    val bondsFile = opt[String](
      default = None,
      descr = "Plain text file consisting of lines of the form `<pk> <stake>`, " +
        "which defines the bond amounts for each validator at genesis. " +
        "<pk> is the public key (in base-16 encoding) identifying the validator and <stake>" +
        "is the amount of Rev they have bonded (an integer). Note: this overrides the --num-validators option."
    )
    val knownValidators = opt[String](
      default = None,
      descr = "Plain text file listing the public keys of validators known to the user (one per line). " +
        "Signatures from these validators are required in order to accept a block which starts the local" +
        "node's view of the blockDAG."
    )
    val walletsFile = opt[String](
      default = None,
      descr = "Plain text file consisting of lines of the form `<algorithm> <pk> <revBalance>`, " +
        "which defines the Rev wallets that exist at genesis. " +
        "<algorithm> is the algorithm used to verify signatures when using the wallet (one of ed25519 or secp256k1)," +
        "<pk> is the public key (in base-16 encoding) identifying the wallet and <revBalance>" +
        "is the amount of Rev in the wallet."
    )

    val bootstrap =
      opt[PeerNode](
        default = Some(
          PeerNode
            .parse("rnode://acd0b05a971c243817a0cfd469f5d1a238c60294@52.119.8.109:40400")
            .right
            .get),
        short = 'b',
        descr = "Bootstrap rnode address for initial seed."
      )(Converter.bootstrapAddressConverter)

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

    def fetchHost(externalAddress: Option[String]): String =
      host.toOption match {
        case Some(host) => host
        case None       => whoami(port(), externalAddress)
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

  private def upnpIpCheck(
      externalAddress: Option[String]): PartialFunction[Unit, (String, String)] =
    Function.unlift(Unit =>
      externalAddress.map(addy => ("UPnP", InetAddress.getByName(addy).getHostAddress)))

  private def checkAll(externalAddress: Option[String]): (String, String) = {
    val func: PartialFunction[Unit, (String, String)] =
      check("AmazonAWS service", "http://checkip.amazonaws.com") orElse
        check("WhatIsMyIP service", "http://bot.whatismyipaddress.com") orElse
        upnpIpCheck(externalAddress) orElse { case _ => ("failed to guess", "localhost") }

    func.apply(())
  }

  private def whoami(port: Int, externalAddress: Option[String]): String = {
    println("INFO - flag --host was not provided, guessing your external IP address")
    val (source, ip) = checkAll(externalAddress)
    println(s"INFO - guessed $ip from source: $source")
    ip
  }

  def casperConf: CasperConf = CasperConf(
    run.validatorPublicKey.toOption,
    run.validatorPrivateKey.toOption,
    run.validatorSigAlgorithm(),
    run.bondsFile.toOption,
    run.knownValidators.toOption,
    run.numValidators(),
    run.data_dir().resolve("genesis"),
    run.walletsFile.toOption,
    run.standalone()
  )

  verify()
}
