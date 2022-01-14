package coop.rchain.node.configuration

import coop.rchain.casper.CasperConf
import coop.rchain.casper.util.comm.ListenAtName.Name
import coop.rchain.comm.PeerNode
import coop.rchain.comm.transport.TlsConf
import coop.rchain.crypto.{PrivateKey, PublicKey}

import java.nio.file.Path
import scala.concurrent.duration.FiniteDuration

final case class NodeConf(
    standalone: Boolean,
    autopropose: Boolean,
    protocolServer: ProtocolServer,
    protocolClient: ProtocolClient,
    peersDiscovery: PeersDiscovery,
    apiServer: ApiServer,
    tls: TlsConf,
    storage: Storage,
    casper: CasperConf,
    metrics: Metrics,
    devMode: Boolean,
    dev: DevConf,
    // This field is dynamic and computed according to profile and is not used directly in client code.
    // But it is required in the model because of how Pureconfig works and how config file is structured (there are
    // references to this key in `defaults.conf`).
    // This `default-data-dir` is initialized from profile provided through CLI and added to default config.
    // So its in HOCON notation that should be loaded into `NodeConf` case class.
    // As we need loader to throw an error when unknown HOCON key is met - this field should also
    // be present in the model.
    defaultDataDir: String,
    disableCostAccounting: Boolean
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
    maxMessageConsumers: Int,
    disableStateExporter: Boolean
)

final case class ProtocolClient(
    networkId: String,
    bootstrap: PeerNode,
    disableLfs: Boolean,
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
    portAdminHttp: Int,
    maxBlocksLimit: Int,
    enableReporting: Boolean,
    keepAliveTime: FiniteDuration,
    keepAliveTimeout: FiniteDuration,
    permitKeepAliveTime: FiniteDuration,
    maxConnectionIdle: FiniteDuration,
    maxConnectionAge: FiniteDuration,
    maxConnectionAgeGrace: FiniteDuration
)

final case class Storage(
    dataDir: Path
)

final case class Metrics(
    prometheus: Boolean,
    influxdb: Boolean,
    influxdbUdp: Boolean,
    zipkin: Boolean,
    sigar: Boolean
)

final case class DevConf(
    deployerPrivateKey: Option[String]
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
final case class Keygen(path: Path)                                        extends Command
final case object LastFinalizedBlock                                       extends Command
final case class IsFinalized(hash: String)                                 extends Command
final case class BondStatus(publicKey: PublicKey)                          extends Command
final case object Help                                                     extends Command
final case class DataAtName(name: Name)                                    extends Command
final case class ContAtName(names: List[Name])                             extends Command
final case object Status                                                   extends Command
