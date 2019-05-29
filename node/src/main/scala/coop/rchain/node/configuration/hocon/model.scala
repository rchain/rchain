package coop.rchain.node.configuration.hocon

import java.nio.file.{Path, Paths}

import scala.concurrent.duration.{Duration, FiniteDuration}

import coop.rchain.casper.CasperConf
import coop.rchain.comm.PeerNode
import coop.rchain.node.configuration
import coop.rchain.shared.StoreType

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
  val Key = s"${Configuration.Key}.server"

  object keys {
    val NetworkId               = "network-id"
    val Bootstrap               = "bootstrap"
    val StoreType               = "store-type"
    val Host                    = "host"
    val HostDynamic             = "host-dynamic"
    val Upnp                    = "upnp"
    val Port                    = "port"
    val PortHttp                = "port-http"
    val PortKademlia            = "port-kademlia"
    val SendTimeout             = "send-timeout"
    val Standalone              = "standalone"
    val DataDir                 = "data-dir"
    val StoreSize               = "store-size"
    val DagStorageSize          = "dag-storage-size"
    val MapSize                 = "map-size"
    val MaxConnections          = "max-connections"
    val AllowPrivateAddresses   = "allow-private-addresses"
    val MaxMessageSize          = "max-message-size"
    val MaxStreamMessageSize    = "max-stream-message-size"
    val PacketChunkSize         = "packet-chunk-size"
    val MessageConsumers        = "message-consumers"
    val FaultToleranceThreshold = "fault-tolerance-threshold"
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
    val storeTypeStr = server.getString(keys.StoreType)
    val storeType = StoreType.from(storeTypeStr) match {
      case Some(st) => st
      case _ =>
        throw new ConfigException.BadValue(
          s"$Key.${keys.StoreType}",
          s"$storeTypeStr is not a supported store type"
        )
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
      defaultTimeout = server.getFiniteDuration(keys.SendTimeout),
      standalone = server.getBoolean(keys.Standalone),
      bootstrap = bootstrap,
      dataDir = server.getPath(keys.DataDir),
      storeType = storeType,
      storeSize = server.getBytes(keys.StoreSize),
      dagStorageSize = server.getBytes(keys.DagStorageSize),
      mapSize = server.getBytes(keys.MapSize),
      maxNumOfConnections = server.getInt(keys.MaxConnections),
      allowPrivateAddresses = server.getBoolean(keys.AllowPrivateAddresses),
      maxMessageSize = server.getBytes(keys.MaxMessageSize).toInt,
      maxStreamMessageSize = server.getBytes(keys.MaxStreamMessageSize),
      packetChunkSize = server.getBytes(keys.PacketChunkSize).toInt,
      messageConsumers = server.getIntOpt(keys.MessageConsumers).getOrElse(messageConsumers),
      faultToleranceThreshold = server.getFloat(keys.FaultToleranceThreshold)
    )
  }
}

object Tls {
  val Key = s"${Server.Key}.tls"

  object keys {
    val Certificate             = "certificate"
    val Key                     = "key"
    val SecureRandomNonBlocking = "secure-random-non-blocking"
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
  val Key = s"${Server.Key}.metrics"

  object keys {
    val Prometheus  = "prometheus"
    val Influxdb    = "influxdb"
    val InfluxdbUdp = "influxdb-udp"
    val Zipkin      = "zipkin"
    val Sigar       = "sigar"
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
  val Key = s"${Configuration.Key}.grpc"

  object keys {
    val Host           = "host"
    val PortExternal   = "port-external"
    val PortInternal   = "port-internal"
    val MaxMessageSize = "max-message-size"
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
  val Key = s"${Configuration.Key}.casper"

  object keys {
    val ValidatorPrivateKey     = "validator-private-key"
    val ValidatorPrivateKeyPath = "validator-private-key-path"
    val ValidatorPublicKey      = "validator-public-key"
    val SigAlgorithm            = "sig-algorithm"
    val BondsFile               = "bonds-file"
    val KnownValidatorsFile     = "known-validators-file"
    val Validators              = "validators"
    val WalletsFile             = "wallets-file"
    val BondMinimum             = "bond-minimum"
    val BondMaximum             = "bond-maximum"
    val HasFaucet               = "has-faucet"
    val RequiredSignatures      = "required-signatures"
    val Shard                   = "shard"
    val GenesisValidator        = "genesis-validator"
    val GenesisApproveInterval  = "genesis-approve-interval"
    val GenesisApproveDuration  = "genesis-approve-duration"
    val DeployTimestamp         = "deploy-timestamp"
    val GenesisPath             = "genesis-path"
  }

  def fromConfig(config: Config): CasperConf = {
    import Configuration.RichConfig

    val casper = config.getConfig(Key)
    val pkPath = casper.getPathOpt(keys.ValidatorPrivateKeyPath).map(Right(_))
    val pk     = casper.getStringOpt(keys.ValidatorPrivateKey).map(Left(_))

    CasperConf(
      publicKeyBase16 = casper.getStringOpt(keys.ValidatorPublicKey),
      privateKey = pkPath.orElse(pk),
      sigAlgorithm = casper.getString(keys.SigAlgorithm),
      bondsFile = casper.getStringOpt(keys.BondsFile),
      knownValidatorsFile = casper.getStringOpt(keys.KnownValidatorsFile),
      numValidators = casper.getInt(keys.Validators),
      walletsFile = casper.getStringOpt(keys.WalletsFile),
      minimumBond = casper.getLong(keys.BondMinimum),
      maximumBond = casper.getLong(keys.BondMaximum),
      hasFaucet = casper.getBoolean(keys.HasFaucet),
      requiredSigs = casper.getInt(keys.RequiredSignatures),
      shardId = casper.getString(keys.Shard),
      approveGenesis = casper.getBoolean(keys.GenesisValidator),
      approveGenesisInterval = casper.getFiniteDuration(keys.GenesisApproveInterval),
      approveGenesisDuration = casper.getFiniteDuration(keys.GenesisApproveDuration),
      deployTimestamp = casper.getLongOpt(keys.DeployTimestamp),
      genesisPath = casper.getPath(keys.GenesisPath),
      createGenesis = false
    )
  }
}
