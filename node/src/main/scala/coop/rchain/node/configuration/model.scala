package coop.rchain.node.configuration

import java.nio.file.Path

import com.typesafe.config.Config

import scala.concurrent.duration.FiniteDuration
import coop.rchain.casper.util.comm.ListenAtName.Name
import coop.rchain.comm.PeerNode
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.casper.CasperConf
import coop.rchain.comm.transport.TlsConf
import coop.rchain.node.configuration.Configuration.Profile
import pureconfig._
import pureconfig.generic.auto._

final case class NodeConf(
    standalone: Boolean,
    protocolServer: ProtocolServer,
    protocolClient: ProtocolClient,
    peersDiscovery: PeersDiscovery,
    apiServer: ApiServer,
    tls: TlsConf,
    storage: Storage,
    casper: CasperConf,
    metrics: Metrics,
    // This field is dynamic and computed according to profile and is not used in client code.
    // But it is required in the model because it is used as a reference in HOCON configuration, and
    // Pureconfig throws an error when finds unknown key.
    defaultDataDir: String
)

final case class ProtocolServer(
    networkId: String,
    host: Option[String],
    allowPrivateAddresses: Boolean,
    useRandomPorts: Boolean,
    dynamicIp: Boolean,
    noUpnp: Boolean,
    port: Int,
    grpcMaxRecvMessageSize: Long,
    grpcMaxRecvStreamMessageSize: Long,
    maxMessageConsumers: Int
)

final case class ProtocolClient(
    networkId: String,
    bootstrap: PeerNode,
    batchMaxConnections: Int,
    networkTimeout: FiniteDuration,
    grpcMaxRecvMessageSize: Long,
    grpcStreamChunkSize: Long
)

final case class PeersDiscovery(
    port: Int,
    lookupInterval: FiniteDuration,
    cleanupInterval: FiniteDuration,
    heartbeatBatchSize: Int,
    initWaitLoopInterval: FiniteDuration
)

final case class ApiServer(
    host: String,
    portGrpcExternal: Int,
    portGrpcInternal: Int,
    grpcMaxRecvMessageSize: Long,
    portHttp: Int,
    maxBlocksLimit: Int,
    enableReporting: Boolean
)

final case class Storage(
    dataDir: Path,
    lmdbMapSizeRspace: Long,
    lmdbMapSizeBlockdagstore: Long,
    lmdbMapSizeBlockstore: Long,
    lmdbMapSizeDeploystore: Long
)

final case class Metrics(
    prometheus: Boolean,
    influxdb: Boolean,
    influxdbUdp: Boolean,
    zipkin: Boolean,
    sigar: Boolean
)

sealed trait Command
final case class Eval(files: List[String], printUnmatchedSendsOnly: Boolean) extends Command
final case object Repl                                                       extends Command
final case class Deploy(
    phloLimit: Long,
    phloPrice: Long,
    validAfterBlock: Long,
    privateKey: Option[PrivateKey],
    privateKeyPath: Option[Path],
    location: String
) extends Command
final case class FindDeploy(id: Array[Byte])                               extends Command
final case class Propose(printUnmatchedSends: Boolean)                     extends Command
final case class ShowBlock(hash: String)                                   extends Command
final case class ShowBlocks(depth: Int)                                    extends Command
final case class VisualizeDag(depth: Int, showJustificationLines: Boolean) extends Command
final case object MachineVerifiableDag                                     extends Command
final case object Run                                                      extends Command
final case class Keygen(algorithm: String, privateKeyPath: Path, publicKeyPath: Path)
    extends Command
final case object LastFinalizedBlock              extends Command
final case class IsFinalized(hash: String)        extends Command
final case class BondStatus(publicKey: PublicKey) extends Command
final case object Help                            extends Command
final case class DataAtName(name: Name)           extends Command
final case class ContAtName(names: List[Name])    extends Command
