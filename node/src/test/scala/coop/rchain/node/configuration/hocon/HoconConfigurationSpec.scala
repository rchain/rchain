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
      |    map-size = 1G
      |    max-connections = 500
      |    max-message-size = 256K
      |  }
      |}
    """.stripMargin

    val expectedServer =
      configuration.Server(
        Some("1.2.3.4"),
        40400,
        40403,
        40404,
        dynamicHostAddress = true,
        noUpnp = true,
        2.seconds,
        PeerNode
          .fromAddress(
            "rnode://de6eed5d00cf080fc587eeb412cb31a75fd10358@52.119.8.109?protocol=40400&discovery=40404"
          )
          .right
          .get,
        standalone = true,
        Paths.get("/root/.rnode"),
        1024 * 1024 * 1024,
        StoreType.LMDB,
        1024 * 1024 * 1024,
        500,
        256 * 1024
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
        Paths.get("/root/node.certificate.pem"),
        Paths.get("/root/node.key.pem"),
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
        |  }
        |}
      """.stripMargin

    val expectedGrpc =
      configuration.GrpcServer(
        "localhost",
        40401,
        40402
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
        Some("111111111111"),
        Some(Right(Paths.get("/root/pk.pem"))),
        "ed25519",
        Some("/root/bonds.txt"),
        Some("/root/validators.txt"),
        5,
        Paths.get("/root/.rnode/genesis"),
        Some("/root/wallet.txt"),
        1L,
        Long.MaxValue,
        hasFaucet = true,
        0,
        "rchain",
        createGenesis = false,
        approveGenesis = true,
        5.seconds,
        5.minutes,
        Some(333)
      )

    val casper = Casper.fromConfig(ConfigFactory.parseString(conf))
    casper shouldEqual expectedCasper
  }
}
