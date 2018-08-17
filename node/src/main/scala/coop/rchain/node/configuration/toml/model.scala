package coop.rchain.node.configuration.toml

import java.nio.file.Path

import coop.rchain.comm.PeerNode

case class Configuration(
    server: Option[Server],
    grpcServer: Option[GrpcServer],
    tls: Option[Tls],
    validators: Option[Validators]
)

case class Server(
    host: Option[String],
    port: Option[Int],
    metricsPort: Option[Int],
    httpPort: Option[Int],
    noUpnp: Option[Boolean],
    defaultTimeout: Option[Int],
    bootstrap: Option[PeerNode],
    standalone: Option[Boolean],
    mapSize: Option[Long],
    inMemoryStore: Option[Boolean],
    casperBlockStoreSize: Option[Long],
    dataDir: Option[Path],
    maxNumOfConnections: Option[Int]
)

case class GrpcServer(
    host: Option[String],
    port: Option[Int],
    portInternal: Option[Int]
)

case class Tls(
    certificate: Option[Path],
    key: Option[Path]
)

case class Validators(
    count: Option[Int],
    bondsFile: Option[String],
    known: Option[String],
    publicKey: Option[String],
    privateKey: Option[String],
    sigAlgorithm: Option[String],
    walletsFile: Option[String],
    shardId: Option[String]
)
