package coop.rchain.node.configuration.commandline

import java.nio.file.Path

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
}

object Options {
  object ports {
    val DefaultPort: Int             = 40400
    val DefaultGrpcPort: Int         = 40401
    val DefaultGrpcPortInternal: Int = 40402
    val DefaultHttpPort: Int         = 40403
    val DefaultKademliaPort: Int     = 40404
  }

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
  import Options.ports._

  version(s"RChain Node ${BuildInfo.version}")
  printedName = "rchain"

  val profile = opt[String](
    name = "profile",
    descr = "Which predefined set of defaults to use: default or docker."
  )

  val configFile = opt[Path](descr = "Path to the configuration file.")

  val grpcPort =
    opt[Int](descr = s"Port used for external gRPC API. Defaults to $DefaultGrpcPort")

  val grpcPortInternal =
    opt[Int](descr = s"Port used for internal gRPC API. Defaults to $DefaultGrpcPortInternal")

  val grpcHost =
    opt[String](descr = "Hostname or IP of node on which gRPC service is running.")

  val grpcMaxMessageSize =
    opt[Int](descr = "Maximum size of message that can be sent via gRPC API")

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

    val network =
      opt[String](descr = "ID of the RChain network")

    val port =
      opt[Int](short = 'p', descr = s"Network port to use. Defaults to $DefaultPort")

    val httpPort =
      opt[Int](descr = s"HTTP port (prometheus, version, status). Defaults to $DefaultHttpPort")

    val kademliaPort =
      opt[Int](
        descr =
          s"Kademlia port used for node discovery based on Kademlia algorithm. Defaults to $DefaultKademliaPort"
      )

    val useRandomPorts = opt[Boolean](
      descr = "Use random ports in case RChain Protocol port and/or Kademlia port are not free."
    )

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

    val epochLength = opt[Int](descr = "the length of the validation epoch in blocks")

    val quarantineLength = opt[Int](descr = "the length of the quarantine time in blocks")

    val numberOfActiveValidators = opt[Int](descr = "the number of the active validators")

    val casperLoopInterval = opt[Int](
      descr =
        "the interval of the casper loop to maintain requested blocks and missing dependent blocks"
    )

    val requestedBlocksTimeout = opt[Int](descr = "the timeout of the requested blocks")

    val bootstrap =
      opt[PeerNode](
        short = 'b',
        descr = "Bootstrap rnode address for initial seed."
      )

    val standalone =
      opt[Flag](short = 's', descr = "Start a stand-alone node (no bootstrapping).")

    val prometheus =
      opt[Flag](descr = "Enable the Prometheus metrics reporter")

    val influxdb =
      opt[Flag](descr = "Enable the InfluxDB metrics reporter")

    val influxdbUdp =
      opt[Flag](descr = "Enable the InfluxDB UDP metrics reporter")

    val zipkin =
      opt[Flag](descr = "Enable the Zipkin span reporter")

    val sigar =
      opt[Flag](descr = "Enable Sigar host system metrics")

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

    val maxNumOfConnections =
      opt[Int](descr = "Number of connected peers picked randomly for broadcasting and streaming")

    val allowPrivateAddresses =
      opt[Flag](descr = "Allow connections to peers with private network addresses")

    val maxMessageSize =
      opt[Int](descr = "Maximum size of message that can be received via transport layer")

    val maxStreamMessageSize =
      opt[Long](descr = "Maximum size of messages that can be received via transport layer streams")

    val packetChunkSize =
      opt[Int](descr = "Chunk size for streaming packets between nodes")

    val messageConsumers =
      opt[Int](descr = "Number of incoming message consumers. Defaults to number of CPU cores")

    val threadPoolSize =
      opt[Int](descr = "Maximum number of threads used by rnode", hidden = true)

    val casperBlockStoreSize =
      opt[Long](required = false, descr = "Casper BlockStore map size (in bytes)")

    val casperBlockDagStorageSize =
      opt[Long](required = false, descr = "Casper BlockDagStorage map size (in bytes)")

    val casperLatestMessagesDataPath =
      opt[Path](required = false, descr = "Path to latest messages data file")

    val casperLatestMessagesCrcPath =
      opt[Path](required = false, descr = "Path to latest messages crc file")

    val validatorPublicKey = opt[String](
      descr = "Base16 encoding of the public key to use for signing a proposed blocks. " +
        "Can be inferred from the private key for some signature algorithms."
    )

    val validatorPrivateKey = opt[String](
      descr = "Base16 encoding of the private key to use for signing a proposed blocks. " +
        "It is not recommended to use in production since private key could be revealed through the process table",
      hidden = true
    )

    val validatorPrivateKeyPath = opt[Path](
      descr = "Path to the base16 encoded private key to use for signing a proposed blocks."
    )

    val shardId = opt[String](
      descr = "Identifier of the shard this node is connected to."
    )

    val faultToleranceThreshold = opt[Float](
      descr = "Float value representing that the node tolerates up " +
        "to fault-tolerance-threshold fraction of the total weight to equivocate."
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

    val maxPeerQueueSize = opt[Int](
      descr = "Fair round robin dispatcher individual peer packet queue size. " +
        "Packets will get dropped by the dispatcher when the queue is full.",
      validate = _ > 0
    )

    val giveUpAfterSkipped = opt[Int](
      descr = "Fair round robin dispatcher give up and try next peer after skipped packets. " +
        "Skipped packets are buffered in other peers packet queues.",
      validate = _ >= 0
    )

    val dropPeerAfterRetries = opt[Int](
      descr = "Fair round robin dispatcher drop inactive peer after round robin rounds. " +
        "After giving up several times the peer gets dropped from the queue.",
      validate = _ >= 0
    )

    val reporting = opt[Flag](descr = "Use this flag to enable reporting endpoints.")

    val finalizationRate = opt[Int](
      descr = "Block finalization is triggered after adding every 'n' blocks. " +
        "Use this option to configure this 'n'."
    )

    val maxNumberOfParents = opt[Int](
      descr = "Maximum number of block parents."
    )

    val maxParentDepth = opt[Int](
      descr = "Maximum depth of block parents."
    )
  }
  addSubcommand(run)

  val keygen = new Subcommand("keygen") {
    descr("Generate a public/private key pair.")

    val algorithm = opt[String](
      descr = "Algorithm to be used for key generation. Must be one of ed25519 or secp256k1.",
      validate = (s: String) => { s == "ed25519" || s == "secp256k1" },
      required = true
    )

    val privateKeyPath = opt[Path](
      descr = "Path to the file where the encoded private key will be written to.",
      required = true
    )
  }
  addSubcommand(keygen)

  val lastFinalizedBlock = new Subcommand("last-finalized-block") {
    descr(
      "View properties of the last finalized block known by Casper on an existing running node."
    )
  }
  addSubcommand(lastFinalizedBlock)

  val isFinalized = new Subcommand("is-finalized") {
    descr(
      "Check if the given block has been finalized by Casper on an existing running node."
    )

    val hash = trailArg[String](
      descr = "The hash value of the block to check",
      required = true
    )
  }
  addSubcommand(isFinalized)

  val repl = new Subcommand("repl") {
    descr("Starts a thin client, that will connect to existing node. See grpcHost and grpcPort.")
  }
  addSubcommand(repl)

  val eval = new Subcommand("eval") {
    descr(
      "Starts a thin client that will evaluate rholang in file on a existing running node. See grpcHost and grpcPort."
    )

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

    val validAfterBlockNumber = opt[Long](
      descr =
        "Set this value to one less than the current block height: you have 50 blocks to get this transaction into the chain."
    )

    val privateKey = opt[PrivateKey](
      descr = "The deployer's ed25519 private key encoded as Base16.",
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

  val visualizeBlocks = new Subcommand("vdag") {
    descr(
      "DAG in DOT format"
    )
    val depth =
      opt[Int](
        name = "depth",
        descr = "depth in terms of block height"
      )
    val showJustificationLines =
      opt[Boolean](
        name = "showJustificationlines",
        descr = "if justification lines should be shown"
      )
  }
  addSubcommand(visualizeBlocks)

  val machineVerifiableDag = new Subcommand("mvdag") {
    descr(
      "Machine Verifiable Dag"
    )
  }
  addSubcommand(machineVerifiableDag)

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

  val findDeploy = new Subcommand("find-deploy") {
    descr(
      "Searches for a block containing the deploy with provided id."
    )
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
  }
  addSubcommand(bondStatus)

  verify()
}
