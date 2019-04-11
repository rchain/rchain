package coop.rchain.node.configuration.hocon

import java.nio.file.Paths

import scala.concurrent.duration._

import coop.rchain.casper.CasperConf
import coop.rchain.comm.PeerNode
import coop.rchain.node.configuration
import coop.rchain.shared.StoreType

import com.typesafe.config.ConfigFactory
import org.scalatest.{FunSuite, Matchers}

class HoconConfigurationSpec extends FunSuite with Matchers {

  test("Parse server section") {
    val conf =
      """
      |rnode {
      |  server {
      |    host = 1.2.3.4
      |    host-dynamic = true
      |    upnp = false
      |    port = 40400
      |    port-http = 40403
      |    port-kademlia = 40404
      |    send-timeout = 2 seconds
      |    standalone = true
      |    bootstrap = "rnode://de6eed5d00cf080fc587eeb412cb31a75fd10358@52.119.8.109?protocol=40400&discovery=40404"
      |    data-dir = /root/.rnode
      |    store-type = lmdb
      |    store-size = 1G
      |    dag-storage-size = 512M
      |    map-size = 1G
      |    max-connections = 500
      |    max-message-size = 256K
      |    packet-chunk-size = 64K
      |    message-consumers = 8
      |  }
      |}
    """.stripMargin

    val expectedServer =
      configuration.Server(
        host = Some("1.2.3.4"),
        port = 40400,
        httpPort = 40403,
        kademliaPort = 40404,
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
        storeType = StoreType.LMDB,
        storeSize = 1024 * 1024 * 1024,
        dagStorageSize = 512 * 1024 * 1024,
        maxNumOfConnections = 500,
        maxMessageSize = 256 * 1024,
        packetChunkSize = 64 * 1024,
        messageConsumers = 8
      )

    val server = Server.fromConfig(ConfigFactory.parseString(conf))
    server shouldEqual expectedServer
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
        |    sig-algorithm = ed25519
        |    bonds-file = /root/bonds.txt
        |    known-validators-file = /root/validators.txt
        |    validators = 5
        |    wallets-file = /root/wallet.txt
        |    bond-minimum = 1
        |    bond-maximum = 9223372036854775807
        |    has-faucet = true
        |    required-signatures = 0
        |    shard = rchain
        |    genesis-validator = true
        |    genesis-approve-interval = 5 seconds
        |    genesis-approve-duration = 5 minutes
        |    genesis-path = /root/.rnode/genesis
        |    deploy-timestamp = 333
        |  }
        |}
      """.stripMargin

    val expectedCasper =
      CasperConf(
        publicKeyBase16 = Some("111111111111"),
        privateKey = Some(Right(Paths.get("/root/pk.pem"))),
        sigAlgorithm = "ed25519",
        bondsFile = Some("/root/bonds.txt"),
        knownValidatorsFile = Some("/root/validators.txt"),
        numValidators = 5,
        genesisPath = Paths.get("/root/.rnode/genesis"),
        walletsFile = Some("/root/wallet.txt"),
        minimumBond = 1L,
        maximumBond = Long.MaxValue,
        hasFaucet = true,
        requiredSigs = 0,
        shardId = "rchain",
        createGenesis = false,
        approveGenesis = true,
        approveGenesisInterval = 5.seconds,
        approveGenesisDuration = 5.minutes,
        deployTimestamp = Some(333)
      )

    val casper = Casper.fromConfig(ConfigFactory.parseString(conf))
    casper shouldEqual expectedCasper
  }
}
