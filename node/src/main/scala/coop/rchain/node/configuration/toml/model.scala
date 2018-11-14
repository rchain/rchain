package coop.rchain.node.configuration.toml

import java.nio.file.Path

import coop.rchain.comm.PeerNode

import scala.concurrent.duration.FiniteDuration

case class Configuration(
    server: Option[Server],
    grpcServer: Option[GrpcServer],
    tls: Option[Tls],
    validators: Option[Validators]
)

case class Server(
    host: Option[String],
    port: Option[Int],
    httpPort: Option[Int],
    kademliaPort: Option[Int],
    dynamicHostAddress: Option[Boolean],
    noUpnp: Option[Boolean],
    defaultTimeout: Option[Int],
    bootstrap: Option[PeerNode],
    standalone: Option[Boolean],
    genesisValidator: Option[Boolean],
    mapSize: Option[Long],
    storeType: Option[String],
    casperBlockStoreSize: Option[Long],
    dataDir: Option[Path],
    maxNumOfConnections: Option[Int],
    maxMessageSize: Option[Int],
    threadPoolSize: Option[Int]
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
    privateKeyPath: Option[Path],
    sigAlgorithm: Option[String],
    walletsFile: Option[String],
    minimumBond: Option[Long],
    maximumBond: Option[Long],
    hasFaucet: Option[Boolean],
    shardId: Option[String],
    requiredSigs: Option[Int],
    approveGenesisDuration: Option[FiniteDuration],
    approveGenesisInterval: Option[FiniteDuration],
    deployTimestamp: Option[Long]
)
