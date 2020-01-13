package coop.rchain.node.configuration.hocon

import java.nio.file.{Path, Paths}

import scala.concurrent.duration.{Duration, FiniteDuration}

import coop.rchain.casper.CasperConf
import coop.rchain.comm.PeerNode
import coop.rchain.node.configuration

import com.typesafe.config.{Config, ConfigException}

object Configuration {
  val Key = "rnode"

  implicit class RichConfig(val underlying: Config) {

    private def getOpt[A](path: String, selector: Config => String => A): Option[A] =
      if (underlying.hasPath(path)) Some(selector(underlying)(path))
      else None

    def getStringOpt(path: String): Option[String] = getOpt(path, _.getString)
    def getLongOpt(path: String): Option[Long]     = getOpt(path, _.getLong)
    def getIntOpt(path: String): Option[Int]       = getOpt(path, _.getInt)
    def getFloat(path: String): Float              = underlying.getDouble(path).toFloat
    def getPath(path: String): Path                = Paths.get(underlying.getString(path))
    def getPathOpt(path: String): Option[Path]     = getOpt(path, _.getPath)
    def getFiniteDuration(path: String): FiniteDuration =
      Duration.fromNanos(underlying.getDuration(path).toNanos)
  }
}

object Server {
  val Key                = s"${Configuration.Key}.server"
  val Keys: List[String] = keys.all.map(k => s"$Key.$k")

  object keys {
    val NetworkId                    = "network-id"
    val Bootstrap                    = "bootstrap"
    val Host                         = "host"
    val HostDynamic                  = "host-dynamic"
    val Upnp                         = "upnp"
    val Port                         = "port"
    val PortHttp                     = "port-http"
    val PortKademlia                 = "port-kademlia"
    val UseRandomPorts               = "use-random-ports"
    val SendTimeout                  = "send-timeout"
    val Standalone                   = "standalone"
    val DataDir                      = "data-dir"
    val StoreSize                    = "store-size"
    val DagStorageSize               = "dag-storage-size"
    val MapSize                      = "map-size"
    val MaxConnections               = "max-connections"
    val AllowPrivateAddresses        = "allow-private-addresses"
    val MaxMessageSize               = "max-message-size"
    val MaxStreamMessageSize         = "max-stream-message-size"
    val PacketChunkSize              = "packet-chunk-size"
    val MessageConsumers             = "message-consumers"
    val FaultToleranceThreshold      = "fault-tolerance-threshold"
    val SynchronyConstraintThreshold = "synchrony-constraint-threshold"
    val HeightConstraintThreshold    = "height-constraint-threshold"
    val Reporting                    = "reporting"

    val all =
      List(
        NetworkId,
        Bootstrap,
        Host,
        HostDynamic,
        Upnp,
        Port,
        PortHttp,
        PortKademlia,
        UseRandomPorts,
        SendTimeout,
        Standalone,
        DataDir,
        StoreSize,
        DagStorageSize,
        MapSize,
        MaxConnections,
        AllowPrivateAddresses,
        MaxMessageSize,
        MaxStreamMessageSize,
        PacketChunkSize,
        MessageConsumers,
        FaultToleranceThreshold,
        SynchronyConstraintThreshold,
        HeightConstraintThreshold,
        Reporting
      )
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def fromConfig(config: Config): configuration.Server = {
    import Configuration.RichConfig

    val server = config.getConfig(Key)
    val bootstrap =
      PeerNode.fromAddress(server.getString(keys.Bootstrap)) match {
        case Right(node) => node
        case Left(error) =>
          throw new ConfigException.BadValue(s"$Key.${keys.Bootstrap}", error.message)
      }
    val messageConsumers = Math.max(Runtime.getRuntime.availableProcessors(), 2)

    configuration.Server(
      networkId = server.getString(keys.NetworkId),
      host = server.getStringOpt(keys.Host),
      dynamicHostAddress = server.getBoolean(keys.HostDynamic),
      noUpnp = !server.getBoolean(keys.Upnp),
      port = server.getInt(keys.Port),
      httpPort = server.getInt(keys.PortHttp),
      kademliaPort = server.getInt(keys.PortKademlia),
      useRandomPorts = server.getBoolean(keys.UseRandomPorts),
      defaultTimeout = server.getFiniteDuration(keys.SendTimeout),
      standalone = server.getBoolean(keys.Standalone),
      bootstrap = bootstrap,
      dataDir = server.getPath(keys.DataDir),
      storeSize = server.getBytes(keys.StoreSize),
      dagStorageSize = server.getBytes(keys.DagStorageSize),
      mapSize = server.getBytes(keys.MapSize),
      maxNumOfConnections = server.getInt(keys.MaxConnections),
      allowPrivateAddresses = server.getBoolean(keys.AllowPrivateAddresses),
      maxMessageSize = server.getBytes(keys.MaxMessageSize).toInt,
      maxStreamMessageSize = server.getBytes(keys.MaxStreamMessageSize),
      packetChunkSize = server.getBytes(keys.PacketChunkSize).toInt,
      messageConsumers = server.getIntOpt(keys.MessageConsumers).getOrElse(messageConsumers),
      faultToleranceThreshold = server.getFloat(keys.FaultToleranceThreshold),
      synchronyConstraintThreshold = server.getDouble(keys.SynchronyConstraintThreshold),
      heightConstraintThreshold = server.getLong(keys.HeightConstraintThreshold),
      reporting = server.getBoolean(keys.Reporting)
    )
  }
}

object RoundRobinDispatcher {
  val Key                = s"${Server.Key}.round-robin-dispatcher"
  val Keys: List[String] = keys.all.map(k => s"$Key.$k")

  object keys {
    val MaxPeerQueueSize     = "max-peer-queue-size"
    val GiveUpAfterSkipped   = "give-up-after-skipped"
    val DropPeerAfterRetries = "drop-peer-after-retries"

    val all =
      List(
        MaxPeerQueueSize,
        GiveUpAfterSkipped,
        DropPeerAfterRetries
      )
  }

  def fromConfig(config: Config): configuration.RoundRobinDispatcher = {

    val rrd = config.getConfig(Key)

    configuration.RoundRobinDispatcher(
      maxPeerQueueSize = rrd.getInt(keys.MaxPeerQueueSize),
      giveUpAfterSkipped = rrd.getInt(keys.GiveUpAfterSkipped),
      dropPeerAfterRetries = rrd.getInt(keys.DropPeerAfterRetries)
    )
  }
}

object Tls {
  val Key                = s"${Server.Key}.tls"
  val Keys: List[String] = keys.all.map(k => s"$Key.$k")

  object keys {
    val Certificate             = "certificate"
    val Key                     = "key"
    val SecureRandomNonBlocking = "secure-random-non-blocking"

    val all =
      List(
        Certificate,
        Key,
        SecureRandomNonBlocking
      )
  }

  def fromConfig(config: Config): configuration.Tls = {
    import Configuration.RichConfig

    val tls = config.getConfig(Key)

    configuration.Tls(
      certificate = tls.getPath(keys.Certificate),
      key = tls.getPath(keys.Key),
      secureRandomNonBlocking = tls.getBoolean(keys.SecureRandomNonBlocking),
      customCertificateLocation = false,
      customKeyLocation = false
    )
  }
}

object Kamon {
  val Key                = s"${Server.Key}.metrics"
  val Keys: List[String] = keys.all.map(k => s"$Key.$k")

  object keys {
    val Prometheus  = "prometheus"
    val Influxdb    = "influxdb"
    val InfluxdbUdp = "influxdb-udp"
    val Zipkin      = "zipkin"
    val Sigar       = "sigar"

    val all =
      List(
        Prometheus,
        Influxdb,
        InfluxdbUdp,
        Zipkin,
        Sigar
      )
  }

  def fromConfig(config: Config): configuration.Kamon = {
    val kamon = config.getConfig(Key)

    configuration.Kamon(
      prometheus = kamon.getBoolean(keys.Prometheus),
      influxDb = kamon.getBoolean(keys.Influxdb),
      influxDbUdp = kamon.getBoolean(keys.InfluxdbUdp),
      zipkin = kamon.getBoolean(keys.Zipkin),
      sigar = kamon.getBoolean(keys.Sigar)
    )
  }
}

object GrpcServer {
  val Key                = s"${Configuration.Key}.grpc"
  val Keys: List[String] = keys.all.map(k => s"$Key.$k")

  object keys {
    val Host           = "host"
    val PortExternal   = "port-external"
    val PortInternal   = "port-internal"
    val MaxMessageSize = "max-message-size"

    val all =
      List(
        Host,
        PortExternal,
        PortInternal,
        MaxMessageSize
      )
  }

  def fromConfig(config: Config): configuration.GrpcServer = {
    val grpc = config.getConfig(Key)

    configuration.GrpcServer(
      host = grpc.getString(keys.Host),
      portExternal = grpc.getInt(keys.PortExternal),
      portInternal = grpc.getInt(keys.PortInternal),
      maxMessageSize = grpc.getBytes(keys.MaxMessageSize).toInt
    )
  }
}

object Casper {
  val Key                = s"${Configuration.Key}.casper"
  val Keys: List[String] = keys.all.map(k => s"$Key.$k")

  object keys {
    val ValidatorPrivateKey      = "validator-private-key"
    val ValidatorPrivateKeyPath  = "validator-private-key-path"
    val ValidatorPublicKey       = "validator-public-key"
    val BondsFile                = "bonds-file"
    val KnownValidatorsFile      = "known-validators-file"
    val Validators               = "validators"
    val WalletsFile              = "wallets-file"
    val BondMinimum              = "bond-minimum"
    val BondMaximum              = "bond-maximum"
    val QuarantineLength         = "quarantine-length"
    val NumberOfActiveValidators = "number-of-active-validators"
    val CasperLoopInterval       = "casper-loop-interval"
    val RequestedBlocksTimeout   = "requested-blocks-timeout"
    val EpochLength              = "epoch-length"
    val RequiredSignatures       = "required-signatures"
    val Shard                    = "shard"
    val GenesisValidator         = "genesis-validator"
    val GenesisApproveInterval   = "genesis-approve-interval"
    val GenesisApproveDuration   = "genesis-approve-duration"
    val DeployTimestamp          = "deploy-timestamp"
    val GenesisPath              = "genesis-path"
    val FinalizationRate         = "finalization-rate"
    val MaxNumberOfParents       = "max-number-of-parents"
    val MaxParentDepth           = "max-parent-depth"

    val all =
      List(
        ValidatorPrivateKey,
        ValidatorPrivateKeyPath,
        ValidatorPublicKey,
        BondsFile,
        KnownValidatorsFile,
        Validators,
        WalletsFile,
        BondMinimum,
        BondMaximum,
        QuarantineLength,
        NumberOfActiveValidators,
        CasperLoopInterval,
        RequestedBlocksTimeout,
        EpochLength,
        RequiredSignatures,
        Shard,
        GenesisValidator,
        GenesisApproveInterval,
        GenesisApproveDuration,
        DeployTimestamp,
        GenesisPath,
        FinalizationRate,
        MaxNumberOfParents,
        MaxParentDepth
      )
  }

  def fromConfig(config: Config): CasperConf = {
    import Configuration.RichConfig

    val casper = config.getConfig(Key)
    val pkPath = casper.getPathOpt(keys.ValidatorPrivateKeyPath).map(Right(_))
    val pk     = casper.getStringOpt(keys.ValidatorPrivateKey).map(Left(_))

    CasperConf(
      publicKeyBase16 = casper.getStringOpt(keys.ValidatorPublicKey),
      privateKey = pkPath.orElse(pk),
      bondsFile = casper.getStringOpt(keys.BondsFile),
      knownValidatorsFile = casper.getStringOpt(keys.KnownValidatorsFile),
      numValidators = casper.getInt(keys.Validators),
      walletsFile = casper.getStringOpt(keys.WalletsFile),
      minimumBond = casper.getLong(keys.BondMinimum),
      maximumBond = casper.getLong(keys.BondMaximum),
      epochLength = casper.getInt(keys.EpochLength),
      quarantineLength = casper.getInt(keys.QuarantineLength),
      numberOfActiveValidators = casper.getInt(keys.NumberOfActiveValidators),
      casperLoopInterval = casper.getInt(keys.CasperLoopInterval),
      requestedBlocksTimeout = casper.getInt(keys.RequestedBlocksTimeout),
      requiredSigs = casper.getInt(keys.RequiredSignatures),
      shardId = casper.getString(keys.Shard),
      approveGenesis = casper.getBoolean(keys.GenesisValidator),
      approveGenesisInterval = casper.getFiniteDuration(keys.GenesisApproveInterval),
      approveGenesisDuration = casper.getFiniteDuration(keys.GenesisApproveDuration),
      deployTimestamp = casper.getLongOpt(keys.DeployTimestamp),
      genesisPath = casper.getPath(keys.GenesisPath),
      createGenesis = false,
      finalizationRate = casper.getInt(keys.FinalizationRate),
      maxNumberOfParents = casper.getInt(keys.MaxNumberOfParents),
      maxParentDepthOpt = casper.getIntOpt(keys.MaxParentDepth)
    )
  }
}
