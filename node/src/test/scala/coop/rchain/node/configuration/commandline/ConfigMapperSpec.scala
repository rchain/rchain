package coop.rchain.node.configuration.commandline

import java.nio.file.Paths

import scala.concurrent.duration._

import coop.rchain.casper.CasperConf
import coop.rchain.comm.PeerNode
import coop.rchain.node.configuration
import coop.rchain.node.configuration.hocon
import coop.rchain.shared.StoreType

import com.typesafe.config.ConfigFactory
import org.scalatest.{FunSuite, Matchers}

class ConfigMapperSpec extends FunSuite with Matchers {

  test("Map server command line arguments to Lightbend config") {
    val args =
      Seq(
        "run",
        "--host 1.2.3.4",
        "--dynamic-host-address",
        "--no-upnp",
        "--port 40400",
        "--http-port 40403",
        "--kademlia-port 40404",
        "--default-timeout 2000",
        "--standalone",
        "--bootstrap rnode://de6eed5d00cf080fc587eeb412cb31a75fd10358@52.119.8.109?protocol=40400&discovery=40404",
        "--data-dir /root/.rnode",
        "--store-type lmdb",
        "--casper-block-store-size 2000",
        "--map-size 1000",
        "--max-num-of-connections 500",
        "--max-message-size 256"
      ).mkString(" ")

    val options = Options(args.split(' '))
    val config  = ConfigMapper.fromOptions(options)

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
        1000,
        StoreType.LMDB,
        2000,
        500,
        256
      )

    val server = hocon.Server.fromConfig(config)
    server shouldEqual expectedServer
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
        Paths.get("/root/node.certificate.pem"),
        Paths.get("/root/node.key.pem"),
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
        "--grpc-port-internal 40402"
      ).mkString(" ")

    val options = Options(args.split(' '))
    val config  = ConfigMapper.fromOptions(options)

    val expectedGrpc =
      configuration.GrpcServer(
        "localhost",
        40401,
        40402
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
        "--validator-sig-algorithm ed25519",
        "--bonds-file /root/bonds.txt",
        "--known-validators /root/validators.txt",
        "--num-validators 5",
        "--wallets-file /root/wallet.txt",
        "--minimum-bond 1",
        "--maximum-bond 1000",
        "--has-faucet",
        "--required-sigs 0",
        "--shard-id rchain",
        "--genesis-validator",
        "--interval 5seconds",
        "--duration 5minutes",
        "--deploy-timestamp 333"
      ).mkString(" ")

    val options              = Options(args.split(' '))
    val config               = ConfigMapper.fromOptions(options)
    val GenesisPathConfigKey = s"${hocon.Casper.Key}.${hocon.Casper.keys.GenesisPath}"
    val defaults =
      ConfigFactory.parseMap(Map(GenesisPathConfigKey -> "/root/.rnode/genesis").asJava)

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
        1000L,
        hasFaucet = true,
        0,
        "rchain",
        createGenesis = false,
        approveGenesis = true,
        5.seconds,
        5.minutes,
        Some(333)
      )

    val casper = hocon.Casper.fromConfig(config.withFallback(defaults))
    casper shouldEqual expectedCasper
  }
}
