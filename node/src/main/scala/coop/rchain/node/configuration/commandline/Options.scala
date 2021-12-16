package coop.rchain.node.configuration.commandline

import java.nio.file.{Path, Paths}

import coop.rchain.casper.util.comm.ListenAtName.{Name, PrivName, PubName}
import coop.rchain.comm.PeerNode
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.node.BuildInfo
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
            .getOrElse(Left("Can not parse the bootstrap address"))
        case Nil => Right(None)
        case _   => Left("Please specify the bootstrap address")
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
              Left(
                s" '${duration}': finite duration is expected, e.g. 20 seconds, 4 minutes, etc.)."
              )
            )(fd => Right(Some(fd)))
          case Nil => Right(None)
          case _   => Left("Provide a duration.")
        }

      override val argType: ArgType.V = ArgType.SINGLE
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
  // This is max number of chars in line for help message
  // Default width is 80 and it does not look good, lines are too short.
  val width = 120

  version(
    s"RChain node | gRPC client \nversion ${BuildInfo.version} (${BuildInfo.gitHeadCommit.getOrElse("commit # unknown")})"
  )
  printedName = "rchain"
  helpWidth(width)

  banner("""
           |Usage: rnode [OPTION]... [SUBCOMMAND] [OPTION]...
           |
           |Options:
           |""".stripMargin)

  val grpcHost = opt[String](
    short = 'h',
    default = Some("localhost"),
    descr = s"Remote gRPC host for client calls. Defaults to localhost."
  )

  val grpcPort = opt[Int](
    short = 'p',
    default = Some(40401),
    descr = s"Remote gRPC host for client calls. Defaults to 40401."
  )

  val grpcInternalPort = opt[Int](
    short = 'i',
    default = Some(40402),
    descr = s"Port for internal gRPC API. Defaults to 40402"
  )

  val grpcMaxRecvMessageSize = opt[Int](
    short = 's',
    default = Some(16 * 1024 * 1024),
    descr = s"Max inbound gRPC message size for client calls. Defaults to 16M."
  )

  val profile = opt[String](
    name = "profile",
    descr = "Predefined set of defaults to use: default or docker."
  )

  val run = new Subcommand(
    "run"
  ) {
    descr(
      "Start RNode server. For defaults see " +
        "https://github.com/rchain/rchain/blob/dev/node/src/main/resources/reference.conf\n"
    )
    helpWidth(width)

    val configFile = opt[Path](
      short = 'c',
      descr = "Path to the configuration file for RNode server. For the reference see " +
        "https://github.com/rchain/rchain/blob/dev/node/src/main/resources/reference.conf"
    )

    val threadPoolSize = opt[Int](
      descr = "Number of threads allocated for main scheduler.",
      hidden = true
    )

    val standalone = opt[Flag](
      short = 's',
      descr = "Start a stand-alone node."
    )

    val bootstrap = opt[PeerNode](
      short = 'b',
      descr = "Address of RNode to bootstrap from when connecting to a network."
    )

    val networkId = opt[String](
      descr = "ID of the RChain network to connect to."
    )

    val autopropose = opt[Flag](
      descr =
        "Make node automatically trying to propose block after new block added or new deploy received."
    )

    val noUpnp = opt[Flag](
      descr = "Use this flag to disable UPnP."
    )

    val dynamicIp = opt[Flag](
      descr = "Host IP address changes dynamically."
    )

    val autogenShardSize = opt[Int](
      descr = "If node has to create genesis block but no bonds file is provided, bonds file with a list of" +
        "random public keys is generated + private keys corresponding to that keys are stored" +
        "in `<genesis-path>/<public_key>.sk`" +
        "This param specifies number of validator identites to generate."
    )
    val disableLfs = opt[Flag](
      descr =
        "Disable the node to start from Last Finalized State, instead it will start from genesis."
    )

    val host = opt[String](
      descr = "Address to bind RChain Protocol server."
    )

    val useRandomPorts = opt[Boolean](
      descr = "Use random ports in case RChain Protocol port and/or Kademlia port are not free."
    )

    val allowPrivateAddresses = opt[Flag](
      descr = "Allow connections to peers with private network addresses."
    )

    val disableStateExporter = opt[Flag](
      descr = "Disable the node respond to export state requests."
    )

    val networkTimeout = opt[FiniteDuration](
      descr = "Default timeout for network calls."
    )

    val discoveryPort = opt[Int](
      descr = s"Port used for node discovery based on Kademlia algorithm. Defaults to 40400"
    )

    val discoveryLookupInterval = opt[FiniteDuration](
      descr = s"Peer discovery interval"
    )

    val discoveryCleanupInterval = opt[FiniteDuration](
      descr = s"Peer discovery interval"
    )

    val discoveryInitWaitLoopInterval = opt[FiniteDuration](
      descr = s"Check for first connection loop interval"
    )

    val discoveryHeartbeatBatchSize = opt[Int](
      descr = s"Peer discovery interval"
    )

    val protocolPort = opt[Int](
      short = 'p',
      descr = s"gRPC port serving Rchain Protocol messages. Defaults to 40404"
    )

    val protocolGrpcMaxRecvMessageSize = opt[Long](
      descr = "Maximum message size for gRPC transport server."
    )

    val protocolGrpcMaxRecvStreamMessageSize = opt[Long](
      descr =
        "Maximum size of messages that can be received via transport layer streams. This limits block size."
    )

    val protocolGrpcStreamChunkSize = opt[Int](
      descr = "Chunk size for streaming packets between nodes"
    )

    val protocolMaxConnections = opt[Int](
      descr = "Number of connected peers picked randomly for broadcasting and streaming"
    )

    val protocolMaxMessageConsumers = opt[Int](
      descr = "Number of incoming message consumers"
    )

    val tlsKeyPath = opt[Path](
      short = 'k',
      descr = "Path to private key for TLS. Elliptic curve secp256r1 key in PEM format is supported. " +
        "If file does not exist, new key will be generated."
    )

    val tlsCertificatePath = opt[Path](
      descr = "Path to X.509 certificate for TLS. If file does not exist, certificate will be " +
        "generated from PEM key located at `tlsKeyPath`."
    )

    val tlsSecureRandomNonBlocking = opt[Flag](
      descr = "Use a non blocking secure random instance"
    )

    val apiHost = opt[String](
      descr = "Address to bind API servers."
    )

    val apiPortGrpcExternal = opt[Int](
      short = 'e',
      descr = s"Port for external gRPC API. Defaults to 40401"
    )

    val apiPortGrpcInternal = opt[Int](
      short = 'i',
      descr = s"Port for internal gRPC API. Defaults to 40402"
    )

    val apiGrpcMaxRecvMessageSize = opt[Int](
      descr = "Maximum message size for gRPC API server. This limits deploy size."
    )

    val apiPortHttp = opt[Int](
      short = 'h',
      descr = s"Port for HTTP services. Defaults to 40403"
    )

    val apiPortAdminHttp = opt[Int](
      short = 'a',
      descr = s"Port for admin HTTP services. Defaults to 40405"
    )

    val apiMaxBlocksLimit = opt[Int](
      descr = "The max block numbers you can aquire from api",
      validate = _ >= 0
    )

    val apiEnableReporting = opt[Flag](
      descr = "Use this flag to enable reporting endpoints."
    )

    val apiKeepAliveTime = opt[FiniteDuration](
      descr = "Sets a custom keepalive time, the delay time for sending next keepalive ping"
    )

    val apiKeepAliveTimeout = opt[FiniteDuration](
      descr = "Sets a custom keepalive timeout, the timeout for keepalive ping requests"
    )

    val apiPermitKeepAliveTime = opt[FiniteDuration](
      descr = "The most aggressive keep-alive time clients are permitted to configure.The server would close" +
        "the connection if clients exceeding this rate "
    )

    val apiMaxConnectionIdle = opt[FiniteDuration](
      descr =
        "Sets a custom max connection idle time, connection being idle for longer than which will be gracefully terminated"
    )

    val apiMaxConnectionAge = opt[FiniteDuration](
      descr =
        "Sets a custom max connection age, connection lasting longer than which will be gracefully terminated"
    )

    val apiMaxConnectionAgeGrace = opt[FiniteDuration](
      descr = "Sets a custom grace time for the graceful connection termination. Once the max connection age" +
        " is reached, RPCs have the grace time to complete. RPCs that do not complete in time will be" +
        " cancelled, allowing the connection to terminate"
    )

    val dataDir = opt[Path](
      required = false,
      descr = "Path to data directory. Defaults to $HOME/.rnode"
    )

    val shardName = opt[String](
      descr = "Name of the shard this node is connected to."
    )

    val faultToleranceThreshold = opt[Float](
      descr = "Float value representing that the node tolerates up " +
        "to fault-tolerance-threshold fraction of the total weight to equivocate."
    )

    val validatorPublicKey = opt[String](
      descr = "Base16 encoding of the public key to use for signing a proposed blocks. " +
        "Can be inferred from the private key for some signature algorithms."
    )

    // We should not encourage exposing private keys in such way, so this param is hidden
    val validatorPrivateKey = opt[String](
      descr = "Base16 encoding of the private key to use for signing a proposed blocks. " +
        "It is not recommended to use in production since private key could be revealed through the process table",
      hidden = true
    )

    val validatorPrivateKeyPath = opt[Path](
      descr = "Path to the base16 encoded private key to use for signing a proposed blocks."
    )

    val casperLoopInterval = opt[FiniteDuration](
      descr =
        "Interval for the casper loop to maintain requested blocks and missing dependent blocks"
    )

    val requestedBlocksTimeout = opt[FiniteDuration](
      descr = "Timeout for blocks requests"
    )

    val finalizationRate = opt[Int](
      descr = "Finalization is called every `n` blocks."
    )

    val maxNumberOfParents = opt[Int](
      descr = "Maximum number of block parents."
    )

    val maxParentDepth = opt[Int](
      descr = "Maximum depth of block parents."
    )

    val forkChoiceStaleThreshold = opt[FiniteDuration](
      descr =
        "Node will request for fork choice tips if the latest FCT is more then " +
          "forkChoiceStaleThreshold old. Default 5 min."
    )

    val forkChoiceCheckIfStaleInterval = opt[FiniteDuration](
      descr = "Interval for check if fork choice tip is stale. Default 1 min."
    )

    val synchronyConstraintThreshold = opt[Float](
      descr = "Float value representing that the node waits until at least " +
        "synchrony-constraint-threshold fraction of the validators (by stake weight) " +
        "proposed at least one block since this node's last proposal."
    )

    val heightConstraintThreshold = opt[Long](
      descr = "Long value representing how far ahead of the last finalized block the node is " +
        "allowed to propose."
    )

    val frrdMaxPeerQueueSize = opt[Int](
      descr = "Fair round robin dispatcher individual peer packet queue size. " +
        "Packets will get dropped by the dispatcher when the queue is full.",
      validate = _ > 0
    )

    val frrdGiveUpAfterSkipped = opt[Int](
      descr = "Fair round robin dispatcher give up and try next peer after skipped packets. " +
        "Skipped packets are buffered in other peers packet queues.",
      validate = _ >= 0
    )

    val frrdDropPeerAfterRetries = opt[Int](
      descr = "Fair round robin dispatcher drop inactive peer after round robin rounds. " +
        "After giving up several times the peer gets dropped from the queue.",
      validate = _ >= 0
    )

    val bondsFile = opt[String](
      descr = "Plain text file consisting of lines of the form `<pk> <stake>`, " +
        "which defines the bond amounts for each validator at genesis. " +
        "<pk> is the public key (in base-16 encoding) identifying the validator and <stake>" +
        "is the amount of Rev they have bonded (an integer). Note: this overrides the --num-validators option."
    )

    val walletsFile = opt[String](
      descr = "Plain text file consisting of lines of the form `<algorithm> <pk> <revBalance>`, " +
        "which defines the Rev wallets that exist at genesis. " +
        "<algorithm> is the algorithm used to verify signatures when using the wallet (one of ed25519 or secp256k1)," +
        "<pk> is the public key (in base-16 encoding) identifying the wallet and <revBalance>" +
        "is the amount of Rev in the wallet."
    )

    val bondMinimum = opt[Long](
      descr = "Minimum bond accepted by the PoS contract in the genesis block."
    )

    val genesisBlockNumber = opt[Long](
      descr = "Configure genesis blockNumber for hard fork."
    )

    val bondMaximum = opt[Long](
      descr = "Maximum bond accepted by the PoS contract in the genesis block."
    )

    val epochLength = opt[Int](
      descr = "the length of the validation epoch in blocks"
    )

    val quarantineLength = opt[Int](
      descr = "the length of the quarantine time in blocks"
    )

    val numberOfActiveValidators = opt[Int](
      descr = "the number of the active validators"
    )

    val requiredSignatures = opt[Int](
      descr =
        "Number of signatures from bonded validators required for Ceremony Master to approve the genesis block."
    )

    val approveInterval = opt[FiniteDuration](
      descr = "Each `approve-interval` Ceremony Master (CM) checks if it have gathered enough signatures to approve" +
        "the genesis block. If positive, CM broadcasts Approved Block, if negative - broadcast Unapproved Block " +
        "one more time and keeps waiting for approvals."
    )

    //TODO remove
    val approveDuration = opt[FiniteDuration](
      descr =
        "Time window in which BlockApproval messages will be accumulated before checking conditions."
    )

    val genesisValidator = opt[Flag](
      descr = "Start a node as a genesis validator."
    )

    val prometheus = opt[Flag](
      descr = "Enable the Prometheus metrics reporter"
    )

    val influxdb = opt[Flag](
      descr = "Enable the InfluxDB metrics reporter"
    )

    val influxdbUdp = opt[Flag](
      descr = "Enable the InfluxDB UDP metrics reporter"
    )

    val zipkin = opt[Flag](
      descr = "Enable the Zipkin span reporter"
    )

    val sigar = opt[Flag](
      descr = "Enable Sigar host system metrics"
    )

    // TODO remove
    val deployTimestamp = opt[Long](
      descr = "Timestamp for the deploys."
    )

    // Dev mode options
    val devMode = opt[Flag](
      descr = "Enable all developer tools."
    )

    val deployerPrivateKey = opt[String](
      descr = "Private key for dummy deploys."
    )

  }
  addSubcommand(run)

  val keygen = new Subcommand("keygen") {
    descr(
      "Generates a public/private key pair. Files created are: \n" +
        "- password protected private key in PEM format, \n" +
        "- public key in PEM format, \n" +
        "- public key in as a HEX string"
    )
    helpWidth(width)

    val location = trailArg[Path](
      descr = "Folder to save keyfies. Defaults to './' ",
      default = Some(Paths.get("")),
      required = false
    )
  }
  addSubcommand(keygen)

  val lastFinalizedBlock = new Subcommand("last-finalized-block") {
    descr(
      "View properties of the last finalized block known by Casper on an existing running node."
    )
    helpWidth(width)
  }
  addSubcommand(lastFinalizedBlock)

  val isFinalized = new Subcommand("is-finalized") {
    descr(
      "Check if the given block has been finalized by Casper on an existing running node."
    )
    helpWidth(width)
    val hash = trailArg[String](
      descr = "The hash value of the block to check",
      required = true
    )
  }
  addSubcommand(isFinalized)

  val repl = new Subcommand("repl") {
    descr(
      "Starts a thin client, that will connect to existing node. See gRPCHost and gRPCPort."
    )
    helpWidth(width)
  }
  addSubcommand(repl)

  val eval = new Subcommand("eval") {
    descr(
      "Starts a thin client that will evaluate rholang in file on a existing running node. See gRPCHost and gRPCPort."
    )
    helpWidth(width)

    val fileNames               = trailArg[List[String]](required = true)(stringListConverter)
    val printUnmatchedSendsOnly = opt[Boolean](required = false)
  }
  addSubcommand(eval)

  val hexCheck: String => Boolean     = _.matches("[0-9a-fA-F]+")
  val addressCheck: String => Boolean = addr => addr.startsWith("0x") && hexCheck(addr.drop(2))

  def validateLength[T](expectedLength: Int)(array: Array[T]): Either[String, Option[Array[T]]] =
    if (array.length == expectedLength)
      Right(Some(array))
    else
      Left(s"Invalid parameter length. Expected length is $expectedLength bytes")

  val deploy = new Subcommand("deploy") {
    descr(
      "Deploy a Rholang source file to Casper on an existing running node. " +
        "The deploy will be packaged and sent as a block to the network depending " +
        "on the configuration of the Casper instance."
    )
    helpWidth(width)

    val phloLimit = opt[Long](
      descr =
        "The amount of phlo to use for the transaction (unused phlo is refunded). Must be positive integer.",
      validate = _ > 0,
      required = true
    )

    val phloPrice = opt[Long](
      descr = "The price of phlo for this transaction in units dust/phlo. Must be positive integer.",
      // Validation here is only for negative numbers, minimum price is check in Block API
      validate = _ >= 0,
      required = true
    )

    val validAfterBlockNumber = opt[Long](
      descr =
        "Set this value to one less than the current block height: you have 50 blocks to get this transaction into the chain."
    )

    val privateKey = opt[PrivateKey](
      descr = "The deployer's secp256k1 private key encoded as Base16.",
      required = false,
      hidden = true
    )(
      Base16Converter
        .flatMap(validateLength(Ed25519.keyLength))
        .map(PrivateKey.apply)
    )

    val privateKeyPath = opt[Path](
      descr = "The deployer's file with encrypted private key.",
      required = false
    )

    val location = trailArg[String](required = true)

  }
  addSubcommand(deploy)

  val showBlock = new Subcommand("show-block") {
    descr(
      "View properties of a block known by Casper on an existing running node." +
        "Output includes: parent hashes, storage contents of the tuplespace."
    )
    helpWidth(width)

    val hash =
      trailArg[String](name = "hash", required = true, descr = "the hash value of the block")
  }
  addSubcommand(showBlock)

  val showBlocks = new Subcommand("show-blocks") {
    descr(
      "View list of blocks in the current Casper view on an existing running node."
    )
    helpWidth(width)

    val depth = opt[Int](
      name = "depth",
      validate = _ > 0,
      descr = "lists blocks to the given depth in terms of block height"
    )

  }
  addSubcommand(showBlocks)

  val visualizeBlocks = new Subcommand("vdag") {
    descr(
      "DAG in DOT format"
    )
    helpWidth(width)

    val depth = opt[Int](
      name = "depth",
      descr = "depth in terms of block height"
    )
    val showJustificationLines = opt[Boolean](
      name = "showJustificationlines",
      descr = "if justification lines should be shown"
    )
  }
  addSubcommand(visualizeBlocks)

  val machineVerifiableDag = new Subcommand("mvdag") {
    descr(
      "Machine Verifiable Dag"
    )
    helpWidth(width)
  }
  addSubcommand(machineVerifiableDag)

  def listenAtName[R](name: String, desc: String)(
      implicit conv: ValueConverter[List[String] => R]
  ) = new Subcommand(name) {
    descr(desc)
    helpWidth(width)

    val typeOfName = opt[List[String] => R](
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

  val findDeploy = new Subcommand("find-deploy") {
    descr(
      "Searches for a block containing the deploy with provided id."
    )
    helpWidth(width)

    val deployId = opt[Array[Byte]](
      descr = "Id of the deploy.",
      required = true
    )(Base16Converter)
  }
  addSubcommand(findDeploy)

  val propose = new Subcommand("propose") {
    descr(
      "Force Casper (on an existing running node) to propose a block based on its accumulated deploys."
    )
    helpWidth(width)

    val printUnmatchedSends = opt[Boolean](required = false)
  }
  addSubcommand(propose)

  val bondStatus = new Subcommand("bond-status") {
    val validatorPublicKey = trailArg[PublicKey](
      descr = "Base16 encoding of the public key to check for bond status.",
      required = true
    )(
      Base16Converter
        .flatMap(validateLength(Ed25519.keyLength))
        .map(PublicKey.apply)
    )
    helpWidth(width)
  }
  addSubcommand(bondStatus)

  val status = new Subcommand("status") {
    descr(
      "Get RNode status information"
    )
    helpWidth(width)
  }
  addSubcommand(status)

  verify()
}
