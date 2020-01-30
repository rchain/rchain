package coop.rchain.node.configuration.hocon

import java.nio.file.Paths

import scala.concurrent.duration._

import coop.rchain.casper.CasperConf
import coop.rchain.comm.PeerNode
import coop.rchain.node.configuration

import com.typesafe.config.ConfigFactory
import org.scalatest.{FunSuite, Matchers}

class HoconConfigurationSpec extends FunSuite with Matchers {

  test("Parse server section") {
    val conf =
      """
      |rnode {
      |  server {
      |    network-id = "testnet"
      |    host = 1.2.3.4
      |    host-dynamic = true
      |    upnp = false
      |    port = 40400
      |    port-http = 40403
      |    port-kademlia = 40404
      |    use-random-ports = true
      |    send-timeout = 2 seconds
      |    standalone = true
      |    bootstrap = "rnode://de6eed5d00cf080fc587eeb412cb31a75fd10358@52.119.8.109?protocol=40400&discovery=40404"
      |    data-dir = /root/.rnode
      |    store-size = 1G
      |    dag-storage-size = 512M
      |    map-size = 1G
      |    max-connections = 500
      |    allow-private-addresses = true
      |    max-message-size = 256K
      |    max-stream-message-size = 200M
      |    packet-chunk-size = 64K
      |    message-consumers = 8
      |    fault-tolerance-threshold = 0.2
      |    synchrony-constraint-threshold = 0.3333333333333333
      |    height-constraint-threshold = 100
      |    reporting = false
      |  }
      |}
    """.stripMargin

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
        mapSize = 1024 * 1024 * 1024,
        storeSize = 1024 * 1024 * 1024,
        dagStorageSize = 512 * 1024 * 1024,
        maxNumOfConnections = 500,
        allowPrivateAddresses = true,
        maxMessageSize = 256 * 1024,
        maxStreamMessageSize = 200 * 1024 * 1024,
        packetChunkSize = 64 * 1024,
        messageConsumers = 8,
        faultToleranceThreshold = 0.2f,
        synchronyConstraintThreshold = 0.3333333333333333d,
        heightConstraintThreshold = 100L,
        reporting = false
      )

    val server = Server.fromConfig(ConfigFactory.parseString(conf))
    server shouldEqual expectedServer
  }

  test("Parse round-robin-dispatcher section") {
    val conf =
      """
        |rnode {
        |  server {
        |    round-robin-dispatcher {
        |      max-peer-queue-size = 10
        |      give-up-after-skipped = 20
        |      drop-peer-after-retries = 30
        |    }
        |  }
        |}
      """.stripMargin

    val expectedRoundRobinDispatcher =
      configuration.RoundRobinDispatcher(
        maxPeerQueueSize = 10,
        giveUpAfterSkipped = 20,
        dropPeerAfterRetries = 30
      )

    val roundRobinDispatcher = RoundRobinDispatcher.fromConfig(ConfigFactory.parseString(conf))
    roundRobinDispatcher shouldEqual expectedRoundRobinDispatcher
  }

  test("Parse tls section") {
    val conf =
      """
      |rnode {
      |  server {
      |    tls {
      |      certificate = /root/node.certificate.pem
      |      key = /root/node.key.pem
      |      secure-random-non-blocking = true
      |    }
      |  }
      |}
      """.stripMargin

    val expectedTls =
      configuration.Tls(
        certificate = Paths.get("/root/node.certificate.pem"),
        key = Paths.get("/root/node.key.pem"),
        customCertificateLocation = false,
        customKeyLocation = false,
        secureRandomNonBlocking = true
      )

    val tls = Tls.fromConfig(ConfigFactory.parseString(conf))
    tls shouldEqual expectedTls
  }

  test("Parse metrics section") {
    val conf =
      """
        |rnode {
        |  server {
        |    metrics {
        |      prometheus = true
        |      influxdb = true
        |      influxdb-udp = true
        |      zipkin = true
        |      sigar = true
        |    }
        |  }
        |}
      """.stripMargin

    val expectedKamon =
      configuration.Kamon(
        prometheus = true,
        influxDb = true,
        influxDbUdp = true,
        zipkin = true,
        sigar = true
      )

    val kamon = Kamon.fromConfig(ConfigFactory.parseString(conf))
    kamon shouldEqual expectedKamon
  }

  test("Parse grpc section") {
    val conf =
      """
        |rnode {
        |  grpc {
        |    host = localhost
        |    port-external = 40401
        |    port-internal = 40402
        |    max-message-size = 4M
        |  }
        |}
      """.stripMargin

    val expectedGrpc =
      configuration.GrpcServer(
        host = "localhost",
        portExternal = 40401,
        portInternal = 40402,
        maxMessageSize = 4 * 1024 * 1024
      )

    val grpc = GrpcServer.fromConfig(ConfigFactory.parseString(conf))
    grpc shouldEqual expectedGrpc
  }

  test("Parse casper section") {
    val conf =
      """
        |rnode {
        |  casper {
        |    validator-public-key = 111111111111
        |    validator-private-key = 222222222222
        |    validator-private-key-path = /root/pk.pem
        |    bonds-file = /root/bonds.txt
        |    known-validators-file = /root/validators.txt
        |    validators = 5
        |    wallets-file = /root/wallet.txt
        |    bond-minimum = 1
        |    bond-maximum = 9223372036854775807
        |    epoch-length = 10000
        |    quarantine-length = 50000
        |    number-of-active-validators = 100
        |    casper-loop-interval = 30
        |    requested-blocks-timeout = 240
        |    required-signatures = 0
        |    shard = rchain
        |    genesis-validator = true
        |    genesis-approve-interval = 5 seconds
        |    genesis-approve-duration = 5 minutes
        |    genesis-path = /root/.rnode/genesis
        |    deploy-timestamp = 333
        |    finalization-rate = 4
        |    max-number-of-parents = 1
        |    max-parent-depth = 7
        |  }
        |}
      """.stripMargin

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
        maximumBond = Long.MaxValue,
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

    val casper = Casper.fromConfig(ConfigFactory.parseString(conf))
    casper shouldEqual expectedCasper
  }
}
