package coop.rchain.node.configuration.toml

import java.nio.file.Paths

import coop.rchain.comm.PeerNode

import org.scalatest._
import scala.concurrent.duration._

class TomlConfigurationSpec extends FunSuite with Matchers {

  import error._

  val config =
    """
      |[server]
      |host = "localhost"
      |port = 10
      |http-port = 12
      |dynamic-host-address = false
      |no-upnp = false
      |default-timeout = 1000
      |bootstrap = "rnode://de6eed5d00cf080fc587eeb412cb31a75fd10358@52.119.8.109?protocol=40400&discovery=40404"
      |standalone = true
      |map-size = 200000000
      |store-type = "lmdb"
      |casper-block-store-size = 2000000
      |data-dir = "/var/rchain"
      |
      |[grpc-server]
      |host = "grpc"
      |port = 12
      |port-internal = 13
      |
      |[tls]
      |certificate = "/tls/certificate.pem"
      |key = "/tls/key.pem"
      |
      |[validators]
      |count = 5
      |bonds-file = "/validator/bonds"
      |known = "v1"
      |public-key = "88888"
      |private-key = "99999"
      |sig-algorithm = "ed25519"
      |wallets-file = "/validator/wallet"
      |required-sigs = 1
      |approve-genesis-duration = "30min"
      |approve-genesis-interval = "1min"
      |deploy-timestamp = 1
      |""".stripMargin

  test("Parse TOML configuration string") {
    val result: Either[TomlConfigurationError, Configuration] = TomlConfiguration.from(config)
    result.isRight shouldEqual true
    val Right(root) = result

    val bootstrap =
      PeerNode
        .fromAddress(
          "rnode://de6eed5d00cf080fc587eeb412cb31a75fd10358@52.119.8.109?protocol=40400&discovery=40404"
        )
        .right
        .get

    // server
    root.server.flatMap(_.host) shouldEqual Some("localhost")
    root.server.flatMap(_.port) shouldEqual Some(10)
    root.server.flatMap(_.httpPort) shouldEqual Some(12)
    root.server.flatMap(_.dynamicHostAddress) shouldEqual Some(false)
    root.server.flatMap(_.noUpnp) shouldEqual Some(false)
    root.server.flatMap(_.defaultTimeout) shouldEqual Some(1000)
    root.server.flatMap(_.bootstrap) shouldEqual Some(bootstrap)
    root.server.flatMap(_.standalone) shouldEqual Some(true)
    root.server.flatMap(_.mapSize) shouldEqual Some(200000000)
    root.server.flatMap(_.storeType) shouldEqual Some("lmdb")
    root.server.flatMap(_.casperBlockStoreSize) shouldEqual Some(2000000)
    root.server.flatMap(_.dataDir) shouldEqual Some(Paths.get("/var/rchain"))

    // grpc
    root.grpcServer.flatMap(_.host) shouldEqual Some("grpc")
    root.grpcServer.flatMap(_.port) shouldEqual Some(12)
    root.grpcServer.flatMap(_.portInternal) shouldEqual Some(13)

    // tls
    root.tls.flatMap(_.certificate) shouldEqual Some(Paths.get("/tls/certificate.pem"))
    root.tls.flatMap(_.key) shouldEqual Some(Paths.get("/tls/key.pem"))

    // validators
    root.validators.flatMap(_.count) shouldEqual Some(5)
    root.validators.flatMap(_.bondsFile) shouldEqual Some("/validator/bonds")
    root.validators.flatMap(_.known) shouldEqual Some("v1")
    root.validators.flatMap(_.publicKey) shouldEqual Some("88888")
    root.validators.flatMap(_.privateKey) shouldEqual Some("99999")
    root.validators.flatMap(_.sigAlgorithm) shouldEqual Some("ed25519")
    root.validators.flatMap(_.walletsFile) shouldEqual Some("/validator/wallet")
    root.validators.flatMap(_.requiredSigs) shouldEqual Some(1)
    root.validators.flatMap(_.approveGenesisDuration) shouldEqual Some(30.minutes)
    root.validators.flatMap(_.approveGenesisInterval) shouldEqual Some(1.minute)
    root.validators.flatMap(_.deployTimestamp) shouldEqual Some(1)
  }

}
