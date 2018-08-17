package coop.rchain.node.configuration.commandline

import java.nio.file.Path

import coop.rchain.comm.PeerNode
import coop.rchain.node.BuildInfo

import org.rogach.scallop._

object Converter {
  import Options._

  implicit val bootstrapAddressConverter: ValueConverter[PeerNode] = new ValueConverter[PeerNode] {
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

  implicit val optionsFlagConverter: ValueConverter[Flag] = new ValueConverter[Flag] {
    def parse(s: List[(String, List[String])]): Either[String, Option[Flag]] =
      flagConverter.parse(s).map(_.map(flag))

    val argType: ArgType.V = ArgType.FLAG
  }
}

object Options {
  import shapeless.tag.@@

  sealed trait FlagTag
  type Flag = Boolean @@ FlagTag

  def flag(b: Boolean): Flag = b.asInstanceOf[Flag]

  implicit def scallopOptionToOption[A](so: ScallopOption[A]): Option[A] = so.toOption

  // We need this conversion because ScallopOption[A] is invariant in A
  implicit def scallopOptionFlagToBoolean(so: ScallopOption[Flag]): ScallopOption[Boolean] =
    so.map(identity)
}

final case class Options(arguments: Seq[String]) extends ScallopConf(arguments) {
  import Options.Flag
  import Converter._

  version(s"RChain Node ${BuildInfo.version}")
  printedName = "rchain"

  val profile = opt[String](name = "profile",
                            descr = "Which predefined set of defaults to use: default or docker.")

  val configFile = opt[Path](descr = "Path to the configuration file.")

  val grpcPort =
    opt[Int](descr = "Port used for external gRPC API.")

  val grpcPortInternal =
    opt[Int](descr = "Port used for internal gRPC API.")

  val grpcHost =
    opt[String](descr = "Hostname or IP of node on which gRPC service is running.")

  val diagnostics = new Subcommand("diagnostics") {
    descr("Node diagnostics")
  }
  addSubcommand(diagnostics)

  val run = new Subcommand("run") {

    val noUpnp = opt[Flag](descr = "Use this flag to disable UpNp.")

    val defaultTimeout =
      opt[Int](descr = "Default timeout for roundtrip connections. Default 1 second.")

    val certificate =
      opt[Path](short = 'c',
                descr =
                  "Path to node's X.509 certificate file, that is being used for identification")

    val key =
      opt[Path](short = 'k',
                descr =
                  "Path to node's private key PEM file, that is being used for TLS communication")

    val secureRandomNonBlocking =
      opt[Flag](descr = "Use a non blocking secure random instance")

    val port =
      opt[Int](short = 'p', descr = "Network port to use.")

    val httpPort =
      opt[Int](descr = "HTTP port (deprecated - all API features will be ported to gRPC API).")

    val metricsPort =
      opt[Int](descr = "Port used by metrics API.")

    val numValidators = opt[Int](descr = "Number of validators at genesis.")
    val bondsFile = opt[String](
      descr = "Plain text file consisting of lines of the form `<pk> <stake>`, " +
        "which defines the bond amounts for each validator at genesis. " +
        "<pk> is the public key (in base-16 encoding) identifying the validator and <stake>" +
        "is the amount of Rev they have bonded (an integer). Note: this overrides the --num-validators option."
    )
    val knownValidators = opt[String](
      descr = "Plain text file listing the public keys of validators known to the user (one per line). " +
        "Signatures from these validators are required in order to accept a block which starts the local" +
        "node's view of the blockDAG."
    )
    val walletsFile = opt[String](
      descr = "Plain text file consisting of lines of the form `<algorithm> <pk> <revBalance>`, " +
        "which defines the Rev wallets that exist at genesis. " +
        "<algorithm> is the algorithm used to verify signatures when using the wallet (one of ed25519 or secp256k1)," +
        "<pk> is the public key (in base-16 encoding) identifying the wallet and <revBalance>" +
        "is the amount of Rev in the wallet."
    )

    val bootstrap =
      opt[PeerNode](
        short = 'b',
        descr = "Bootstrap rnode address for initial seed."
      )

    val standalone =
      opt[Flag](short = 's', descr = "Start a stand-alone node (no bootstrapping).")

    val host = opt[String](descr = "Hostname or IP of this node.")

    val data_dir =
      opt[Path](required = false, descr = "Path to data directory. Defaults to $HOME/.rnode")

    val map_size = opt[Long](required = false, descr = "Map size (in bytes)")

    val inMemoryStore = opt[Boolean](required = false, descr = "Use in-memory store beneath RSpace")

    val maxNumOfConnections =
      opt[Int](descr = "Maximum number of peers allowed to connect to the node")

    val casperBlockStoreSize =
      opt[Long](required = false, descr = "Casper BlockStore map size (in bytes)")

    val validatorPublicKey = opt[String](
      descr = "Base16 encoding of the public key to use for signing a proposed blocks. " +
        "Can be inferred from the private key for some signature algorithms."
    )

    val validatorPrivateKey = opt[String](
      descr = "Base16 encoding of the private key to use for signing a proposed blocks.")

    val validatorSigAlgorithm = opt[String](
      descr = "Name of the algorithm to use for signing proposed blocks. " +
        "Currently supported values: ed25519")

    val shardId = opt[String](
      descr = "Identifier of the shard this node is connected to."
    )
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

    val addressCheck: String => Boolean = addr =>
      addr.startsWith("0x") && addr.drop(2).matches("[0-9a-fA-F]+")
    val from = opt[String](
      descr = "Purse address that will be used to pay for the deployment.",
      required = true,
      validate = addressCheck
    )

    val phloLimit = opt[Int](
      descr = "The amount of phlo to use for the transaction (unused phlo is refunded).",
      required = true
    )

    val phloPrice = opt[Int](
      descr = "The price of phlo for this transaction in units dust/phlo.",
      required = true
    )

    val nonce = opt[Int](
      descr = "This allows to overwrite your own pending transactions that use the same nonce.",
      required = true
    )

    val location = trailArg[String](required = true)
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

  verify()
}
