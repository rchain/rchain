package coop.rchain.node.configuration.toml

import java.nio.file.Path

import coop.rchain.comm.PeerNode

case class TomlRoot(
    server: Option[TomlServer],
    grpcServer: Option[TomlGrpcServer],
    tls: Option[TomlTls],
    validators: Option[TomlValidators]
)

case class TomlServer(
    host: Option[String],
    port: Option[Int],
    metricsPort: Option[Int],
    noUpnp: Option[Boolean],
    defaultTimeout: Option[Int],
    bootstrap: Option[PeerNode],
    standalone: Option[Boolean],
    mapSize: Option[Long]
)

case class TomlGrpcServer(
    host: Option[String],
    port: Option[Int]
)

case class TomlTls(
    certificate: Option[Path],
    key: Option[Path]
)

case class TomlValidators(
    count: Option[Int],
    bondsFile: Option[String],
    known: Option[String],
    publicKey: Option[String],
    privateKey: Option[String],
    sigAlgorithm: Option[String],
    walletsFile: Option[String]
)
