package coop.rchain.node.configuration.commandline

import java.nio.file.Paths

import scala.concurrent.duration._

import coop.rchain.casper.CasperConf
import coop.rchain.comm.PeerNode
import coop.rchain.node.configuration
import coop.rchain.node.configuration.hocon

import com.typesafe.config.ConfigFactory
import org.scalatest.{FunSuite, Matchers}

class ConfigMapperSpec extends FunSuite with Matchers {

  test("Map server command line arguments to Lightbend config") {
    val args =
      Seq(
        "run",
        "--network testnet",
        "--host 1.2.3.4",
        "--dynamic-host-address",
        "--no-upnp",
        "--port 40400",
        "--http-port 40403",
        "--kademlia-port 40404",
        "--use-random-ports",
        "--default-timeout 2000",
        "--standalone",
        "--bootstrap rnode://de6eed5d00cf080fc587eeb412cb31a75fd10358@52.119.8.109?protocol=40400&discovery=40404",
        "--data-dir /root/.rnode",
        "--casper-block-store-size 2000",
        "--casper-block-dag-storage-size 3000",
        "--map-size 1000",
        "--max-num-of-connections 500",
        "--allow-private-addresses",
        "--packet-chunk-size 64",
        "--max-message-size 256",
        "--max-stream-message-size 200",
        "--message-consumers 8",
        "--fault-tolerance-threshold 0.2",
        "--synchrony-constraint-threshold 0.0",
        "--height-constraint-threshold 100",
        "--reporting"
      ).mkString(" ")

    val options = Options(args.split(' '))
    val config  = ConfigMapper.fromOptions(options)

    val expectedServer =
      configuration.Server(
        networkId = "testnet",
        host = Some("1.2.3.4"),
        port = 40400,
        httpPort = 40403,
        kademliaPort = 40404,
        useRandomPorts = true,
        dynamicHostAddress = true,
        noUpnp = true,
        defaultTimeout = 2.seconds,
        bootstrap = PeerNode
          .fromAddress(
            "rnode://de6eed5d00cf080fc587eeb412cb31a75fd10358@52.119.8.109?protocol=40400&discovery=40404"
          )
          .right
          .get,
        standalone = true,
        dataDir = Paths.get("/root/.rnode"),
        mapSize = 1000,
        storeSize = 2000,
        dagStorageSize = 3000,
        maxNumOfConnections = 500,
        allowPrivateAddresses = true,
        maxMessageSize = 256,
        maxStreamMessageSize = 200,
        packetChunkSize = 64,
        messageConsumers = 8,
        faultToleranceThreshold = 0.2f,
        synchronyConstraintThreshold = 0.0d,
        heightConstraintThreshold = 100L,
        reporting = true
      )

    val server = hocon.Server.fromConfig(config)
    server shouldEqual expectedServer
  }

  test("Map round robin dispatcher command line arguments to Lightbend config") {
    val args =
      Seq(
        "run",
        "--max-peer-queue-size 10",
        "--give-up-after-skipped 20",
        "--drop-peer-after-retries 30"
      ).mkString(" ")

    val options = Options(args.split(' '))
    val config  = ConfigMapper.fromOptions(options)

    val expectedRoundRobinDispatcher =
      configuration.RoundRobinDispatcher(
        maxPeerQueueSize = 10,
        giveUpAfterSkipped = 20,
        dropPeerAfterRetries = 30
      )

    val roundRobinDispatcher = hocon.RoundRobinDispatcher.fromConfig(config)
    roundRobinDispatcher shouldEqual expectedRoundRobinDispatcher
  }

  test("Map TLS command line arguments to Lightbend config") {
    val args =
      Seq(
        "run",
        "--certificate /root/node.certificate.pem",
        "--key /root/node.key.pem",
        "--secure-random-non-blocking"
      ).mkString(" ")

    val options = Options(args.split(' '))
    val config  = ConfigMapper.fromOptions(options)

    val expectedTls =
      configuration.Tls(
        certificate = Paths.get("/root/node.certificate.pem"),
        key = Paths.get("/root/node.key.pem"),
        customCertificateLocation = false,
        customKeyLocation = false,
        secureRandomNonBlocking = true
      )

    val tls = hocon.Tls.fromConfig(config)
    tls shouldEqual expectedTls
  }

  test("Map metrics command line arguments to Lightbend config") {
    val args =
      Seq(
        "run",
        "--prometheus",
        "--influxdb",
        "--influxdb-udp",
        "--zipkin",
        "--sigar"
      ).mkString(" ")

    val options = Options(args.split(' '))
    val config  = ConfigMapper.fromOptions(options)

    val expectedKamon =
      configuration.Kamon(
        prometheus = true,
        influxDb = true,
        influxDbUdp = true,
        zipkin = true,
        sigar = true
      )

    val kamon = hocon.Kamon.fromConfig(config)
    kamon shouldEqual expectedKamon
  }

  test("Map gRPC command line arguments to Lightbend config") {
    val args =
      Seq(
        "--grpc-host localhost",
        "--grpc-port 40401",
        "--grpc-port-internal 40402",
        "--grpc-max-message-size 256"
      ).mkString(" ")

    val options = Options(args.split(' '))
    val config  = ConfigMapper.fromOptions(options)

    val expectedGrpc =
      configuration.GrpcServer(
        host = "localhost",
        portExternal = 40401,
        portInternal = 40402,
        maxMessageSize = 256
      )

    val grpc = hocon.GrpcServer.fromConfig(config)
    grpc shouldEqual expectedGrpc
  }

  test("Map Casper command line arguments to Lightbend config") {
    import collection.JavaConverters._
    val args =
      Seq(
        "run",
        "--validator-public-key 111111111111",
        "--validator-private-key 222222222222",
        "--validator-private-key-path /root/pk.pem",
        "--bonds-file /root/bonds.txt",
        "--known-validators /root/validators.txt",
        "--num-validators 5",
        "--wallets-file /root/wallet.txt",
        "--minimum-bond 1",
        "--maximum-bond 1000",
        "--epoch-length 10000",
        "--quarantine-length 50000",
        "--number-of-active-validators 100",
        "--casper-loop-interval 30",
        "--requested-blocks-timeout 240",
        "--required-sigs 0",
        "--shard-id rchain",
        "--genesis-validator",
        "--interval 5seconds",
        "--duration 5minutes",
        "--deploy-timestamp 333",
        "--finalization-rate 4",
        "--max-number-of-parents 1",
        "--max-parent-depth 7"
      ).mkString(" ")

    val options              = Options(args.split(' '))
    val config               = ConfigMapper.fromOptions(options)
    val GenesisPathConfigKey = s"${hocon.Casper.Key}.${hocon.Casper.keys.GenesisPath}"
    val defaults =
      ConfigFactory.parseMap(Map(GenesisPathConfigKey -> "/root/.rnode/genesis").asJava)

    val expectedCasper =
      CasperConf(
        publicKeyBase16 = Some("111111111111"),
        privateKey = Some(Right(Paths.get("/root/pk.pem"))),
        bondsFile = Some("/root/bonds.txt"),
        knownValidatorsFile = Some("/root/validators.txt"),
        numValidators = 5,
        genesisPath = Paths.get("/root/.rnode/genesis"),
        walletsFile = Some("/root/wallet.txt"),
        minimumBond = 1L,
        maximumBond = 1000L,
        epochLength = 10000,
        quarantineLength = 50000,
        numberOfActiveValidators = 100,
        casperLoopInterval = 30,
        requestedBlocksTimeout = 240,
        requiredSigs = 0,
        shardId = "rchain",
        createGenesis = false,
        approveGenesis = true,
        approveGenesisInterval = 5.seconds,
        approveGenesisDuration = 5.minutes,
        deployTimestamp = Some(333),
        finalizationRate = 4,
        maxNumberOfParents = 1,
        maxParentDepthOpt = Some(7)
      )

    val casper = hocon.Casper.fromConfig(config.withFallback(defaults))
    casper shouldEqual expectedCasper
  }
}
