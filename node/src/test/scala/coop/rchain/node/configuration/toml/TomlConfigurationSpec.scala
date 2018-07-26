package coop.rchain.node.configuration.toml

import java.nio.file.Paths

import coop.rchain.comm.PeerNode

import org.scalatest._

class TomlConfigurationSpec extends FunSuite with Matchers {

  val config =
    """
      |[server]
      |host = "localhost"
      |port = 10
      |metrics-port = 11
      |no-upnp = false
      |default-timeout = 1000
      |bootstrap = "rnode://acd0b05a971c243817a0cfd469f5d1a238c60294@52.119.8.109:40400"
      |standalone = true
      |map-size = 200000000
      |
      |[grpc-server]
      |host = "grpc"
      |port = 12
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
    """.stripMargin

  test("Parse TOML configuration string") {
    val result: Either[String, TomlRoot] = TomlConfiguration.from(config)
    result.isRight shouldEqual true
    val Right(root) = result

    val bootstrap =
      PeerNode
        .parse("rnode://acd0b05a971c243817a0cfd469f5d1a238c60294@52.119.8.109:40400")
        .right
        .get

    // server
    root.server.flatMap(_.host) shouldEqual Some("localhost")
    root.server.flatMap(_.port) shouldEqual Some(10)
    root.server.flatMap(_.metricsPort) shouldEqual Some(11)
    root.server.flatMap(_.noUpnp) shouldEqual Some(false)
    root.server.flatMap(_.defaultTimeout) shouldEqual Some(1000)
    root.server.flatMap(_.bootstrap) shouldEqual Some(bootstrap)
    root.server.flatMap(_.standalone) shouldEqual Some(true)
    root.server.flatMap(_.mapSize) shouldEqual Some(200000000)

    // grpc
    root.grpcServer.flatMap(_.host) shouldEqual Some("grpc")
    root.grpcServer.flatMap(_.port) shouldEqual Some(12)

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
  }

}
