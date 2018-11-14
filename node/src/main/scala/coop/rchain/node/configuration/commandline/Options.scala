package coop.rchain.node.configuration.commandline

import java.nio.file.Path

import coop.rchain.casper.util.comm.ListenAtName.{Name, PrivName, PubName}
import coop.rchain.comm.PeerNode
import coop.rchain.node.BuildInfo
import coop.rchain.shared.StoreType
import org.rogach.scallop._

import scala.concurrent.duration.{Duration, FiniteDuration}

object Converter {
  import Options._

  implicit val bootstrapAddressConverter: ValueConverter[PeerNode] = new ValueConverter[PeerNode] {
    def parse(s: List[(String, List[String])]): Either[String, Option[PeerNode]] =
      s match {
        case (_, uri :: Nil) :: Nil =>
          PeerNode
            .fromAddress(uri)
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

  private def nameConverter[A](
      onPub: List[String] => A,
      onPriv: List[String] => A,
      argType0: ArgType.V
  ) = new ValueConverter[List[String] => A] {
    import cats.instances.either._
    import cats.instances.option._
    import cats.syntax.traverse._

    override def parse(s: List[(String, List[String])]) = {
      val optMap  = s.toMap
      val typeOpt = optMap.get("type").orElse(optMap.get("t"))

      typeOpt.traverse[Either[String, ?], List[String] => A] {
        case "priv" :: _ => Right(onPriv)
        case "pub" :: _  => Right(onPub)
        case _           => Left("Bad option value. Use \"pub\" or \"priv\"")
      }
    }

    override val argType = argType0
  }

  implicit val nameProviderConverter =
    nameConverter[Name](names => PubName(names.head), names => PrivName(names.head), ArgType.SINGLE)

  implicit val namesProviderConverter =
    nameConverter[List[Name]](_.map(PubName), _.map(PrivName), ArgType.LIST)

  implicit val finiteDurationConverter: ValueConverter[FiniteDuration] =
    new ValueConverter[FiniteDuration] {

      override def parse(s: List[(String, List[String])]): Either[String, Option[FiniteDuration]] =
        s match {
          case (_, duration :: Nil) :: Nil =>
            val finiteDuration = Some(Duration(duration)).collect { case f: FiniteDuration => f }
            finiteDuration.fold[Either[String, Option[FiniteDuration]]](
              Left("Expected finite duration.")
            )(fd => Right(Some(fd)))
          case Nil => Right(None)
          case _   => Left("Provide a duration.")
        }

      override val argType: ArgType.V = ArgType.SINGLE
    }

  implicit val storeTypeConverter: ValueConverter[StoreType] = new ValueConverter[StoreType] {
    def parse(s: List[(String, List[String])]): Either[String, Option[StoreType]] =
      s match {
        case (_, storeType :: Nil) :: Nil =>
          StoreType
            .from(storeType)
            .map(u => Right(Some(u)))
            .getOrElse(Left("can't parse the store type"))
        case Nil => Right(None)
        case _   => Left("provide the store type")
      }
    val argType: ArgType.V = ArgType.SINGLE
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
  import Converter._
  import Options.Flag

  version(s"RChain Node ${BuildInfo.version}")
  printedName = "rchain"

  val profile = opt[String](
    name = "profile",
    descr = "Which predefined set of defaults to use: default or docker."
  )

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

    val dynamicHostAddress = opt[Flag](descr = "Host IP address changes dynamically")

    val noUpnp = opt[Flag](descr = "Use this flag to disable UpNp.")

    val defaultTimeout =
      opt[Int](descr = "Default timeout for roundtrip connections. Default 1 second.")

    val certificate =
      opt[Path](
        short = 'c',
        descr = "Path to node's X.509 certificate file, that is being used for identification"
      )

    val key =
      opt[Path](
        short = 'k',
        descr = "Path to node's private key PEM file, that is being used for TLS communication"
      )

    val secureRandomNonBlocking =
      opt[Flag](descr = "Use a non blocking secure random instance")

    val port =
      opt[Int](short = 'p', descr = "Network port to use.")

    val httpPort =
      opt[Int](descr = "HTTP port (deprecated - all API features will be ported to gRPC API).")

    val kademliaPort =
      opt[Int](descr = "Kademlia port used for node discovery based on Kademlia algorithm")

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
    val minimumBond = opt[Long](
      descr = "Minimum bond accepted by the PoS contract in the genesis block."
    )
    val maximumBond = opt[Long](
      descr = "Maximum bond accepted by the PoS contract in the genesis block."
    )
    val hasFaucet = opt[Boolean](
      descr = "True if there should be a public access Rev faucet in the genesis block."
    )

    val bootstrap =
      opt[PeerNode](
        short = 'b',
        descr = "Bootstrap rnode address for initial seed."
      )

    val standalone =
      opt[Flag](short = 's', descr = "Start a stand-alone node (no bootstrapping).")

    val requiredSigs =
      opt[Int](
        descr =
          "Number of signatures from trusted validators required to creating an approved genesis block."
      )

    val deployTimestamp =
      opt[Long](
        descr = "Timestamp for the deploys."
      )

    val duration =
      opt[FiniteDuration](
        short = 'd',
        descr =
          "Time window in which BlockApproval messages will be accumulated before checking conditions."
      )

    val interval =
      opt[FiniteDuration](
        short = 'i',
        descr = "Interval at which condition for creating ApprovedBlock will be checked."
      )

    val genesisValidator =
      opt[Flag](descr = "Start a node as a genesis validator.")

    val host = opt[String](descr = "Hostname or IP of this node.")

    val dataDir =
      opt[Path](required = false, descr = "Path to data directory. Defaults to $HOME/.rnode")

    val mapSize = opt[Long](required = false, descr = "Map size (in bytes)")

    val storeType = opt[StoreType](required = false, descr = "Type of RSpace backing store")

    val maxNumOfConnections =
      opt[Int](descr = "Maximum number of peers allowed to connect to the node")

    val maxMessageSize =
      opt[Int](descr = "Maximum size of message that can be sent via transport layer")

    val threadPoolSize =
      opt[Int](descr = "Maximum number of threads used by rnode", hidden = true)

    val casperBlockStoreSize =
      opt[Long](required = false, descr = "Casper BlockStore map size (in bytes)")

    val validatorPublicKey = opt[String](
      descr = "Base16 encoding of the public key to use for signing a proposed blocks. " +
        "Can be inferred from the private key for some signature algorithms."
    )

    val validatorPrivateKey = opt[String](
      descr = "Base16 encoding of the private key to use for signing a proposed blocks. " +
        "It is not recommended to use in production since private key could be revealed through the process table"
    )

    val validatorPrivateKeyPath = opt[Path](
      descr = "Path to the base16 encoded private key to use for signing a proposed blocks."
    )

    val validatorSigAlgorithm = opt[String](
      descr = "Name of the algorithm to use for signing proposed blocks. " +
        "Currently supported values: ed25519"
    )

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
      "Starts a thin client that will evaluate rholang in file on a existing running node. See grpcHost and grpcPort."
    )

    val fileNames = trailArg[List[String]](required = true)(stringListConverter)
  }
  addSubcommand(eval)

  val deployDemo = new Subcommand("deploy-demo") {
    descr(
      "Demo sending some placeholder Deploy operations to Casper on an existing running node at regular intervals"
    )
  }
  addSubcommand(deployDemo)

  val hexCheck: String => Boolean     = _.matches("[0-9a-fA-F]+")
  val addressCheck: String => Boolean = addr => addr.startsWith("0x") && hexCheck(addr.drop(2))

  val deploy = new Subcommand("deploy") {
    descr(
      "Deploy a Rholang source file to Casper on an existing running node. " +
        "The deploy will be packaged and sent as a block to the network depending " +
        "on the configuration of the Casper instance."
    )

    val from = opt[String](
      descr = "Purse address that will be used to pay for the deployment.",
      validate = addressCheck
    )

    val phloLimit =
      opt[Long](
        descr =
          "The amount of phlo to use for the transaction (unused phlo is refunded). Must be positive integer.",
        validate = _ > 0,
        required = true
      )

    val phloPrice = opt[Long](
      descr = "The price of phlo for this transaction in units dust/phlo. Must be positive integer.",
      validate = _ > 0,
      required = true
    )

    val nonce = opt[Int](
      descr = "This allows you to overwrite your own pending transactions that use the same nonce."
    )

    val location = trailArg[String](required = true)
  }
  addSubcommand(deploy)

  val showBlock = new Subcommand("show-block") {
    descr(
      "View properties of a block known by Casper on an existing running node." +
        "Output includes: parent hashes, storage contents of the tuplespace."
    )
    val hash =
      trailArg[String](name = "hash", required = true, descr = "the hash value of the block")
  }
  addSubcommand(showBlock)

  val showBlocks = new Subcommand("show-blocks") {
    descr(
      "View list of blocks in the current Casper view on an existing running node."
    )
    val depth =
      opt[Int](
        name = "depth",
        validate = _ > 0,
        descr = "lists blocks to the given depth in terms of block height"
      )

  }
  addSubcommand(showBlocks)

  def listenAtName[R](name: String, desc: String)(
      implicit conv: ValueConverter[List[String] => R]
  ) = new Subcommand(name) {
    descr(desc)

    val typeOfName =
      opt[List[String] => R](
        required = true,
        descr = "Type of the specified name",
        name = "type",
        short = 't'
      )

    val content =
      opt[List[String]](required = true, descr = "Rholang name", name = "content", short = 'c')

    val name: ScallopOption[R] =
      for {
        content <- content
        f       <- typeOfName
      } yield f(content)
  }

  val dataAtName =
    listenAtName[Name]("listen-data-at-name", "Listen for data at the specified name")

  val contAtName =
    listenAtName[List[Name]]("listen-cont-at-name", "Listen for continuation at the specified name")

  addSubcommand(dataAtName)
  addSubcommand(contAtName)

  val propose = new Subcommand("propose") {
    descr(
      "Force Casper (on an existing running node) to propose a block based on its accumulated deploys."
    )
  }
  addSubcommand(propose)

  val bondingDeployGen = new Subcommand("generateBondingDeploys") {
    descr(
      "Creates the rholang source files needed for bonding assuming you have a " +
        "pre-wallet from the REV issuance. These files must be" +
        "deployed to a node operated by a presently bonded validator. The rho files" +
        "are created in the working directory where the command is executed. Note: " +
        "for security reasons it is best to deploy `unlock*.rho` and `forward*.rho` first" +
        "and `bond*.rho` in a separate block after those (i.e. only deploy `bond*.rho` " +
        "after `unlock*.rho` and `forward*.rho` have safely been included in a propsed block)."
    )

    val ethAddr = opt[String](
      descr = "Ethereum address associated with the \"pre-wallet\" to bond.",
      validate = addressCheck,
      required = true
    )

    val bondKey = opt[String](
      descr = "Hex-encoded public key which will be used as the validator idenity after bonding. " +
        "Note: as of this version of node this must be an ED25519 key.",
      validate = hexCheck,
      required = true
    )

    val amount = opt[Long](
      descr = "The amount of REV to bond. Must be less than or equal to the wallet balance.",
      validate = _ > 0,
      required = true
    )

    val publicKey = opt[String](
      descr = "Hex-encoded public key associated with the Ethereum address of the pre-wallet.",
      validate = hexCheck,
      required = true
    )

    val privateKey = opt[String](
      descr = "Hex-encoded private key associated with the Ethereum address of the pre-wallet.",
      validate = hexCheck,
      required = true
    )
  }
  addSubcommand(bondingDeployGen)

  val faucetBondingDeployGen = new Subcommand("generateFaucetBondingDeploys") {
    descr(
      "Creates the rholang source files needed for bonding by making use of " +
        "test net faucet. These files must be" +
        "deployed to a node operated by a presently bonded validator. The rho files" +
        "are created in the working directory where the command is executed. Note: " +
        "for security reasons it is best to deploy `forward*.rho` first" +
        "and then `bond*.rho` in a separate block afterwards (i.e. only deploy `bond*.rho` " +
        "after `forward*.rho` has safely been included in a propsed block)."
    )

    val amount = opt[Long](
      descr = "The amount of REV to bond. Must be less than or equal to the wallet balance.",
      validate = _ > 0,
      required = true
    )

    val sigAlgorithm = opt[String](
      descr =
        "Signature algorithm to be used with the provided keys. Must be one of ed25519 or secp256k1.",
      validate = (s: String) => { s == "ed25519" || s == "secp256k1" },
      required = true
    )

    val publicKey = opt[String](
      descr = "Hex-encoded public key to be used as the validator id when bonding.",
      validate = hexCheck,
      required = true
    )

    val privateKey = opt[String](
      descr = "Hex-encoded private key associated with the supplied public key.",
      validate = hexCheck,
      required = true
    )
  }
  addSubcommand(faucetBondingDeployGen)

  verify()
}
