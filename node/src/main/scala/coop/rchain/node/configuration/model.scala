package coop.rchain.node.configuration

import java.nio.file.Path

import scala.concurrent.duration.FiniteDuration

import coop.rchain.casper.util.comm.ListenAtName.Name
import coop.rchain.comm.PeerNode
import coop.rchain.crypto.PrivateKey
import coop.rchain.shared.StoreType

final case class Server(
    networkId: String,
    host: Option[String],
    port: Int,
    httpPort: Int,
    kademliaPort: Int,
    useRandomPorts: Boolean,
    dynamicHostAddress: Boolean,
    noUpnp: Boolean,
    defaultTimeout: FiniteDuration,
    bootstrap: PeerNode,
    standalone: Boolean,
    dataDir: Path,
    mapSize: Long,
    storeType: StoreType,
    storeSize: Long,
    dagStorageSize: Long,
    maxNumOfConnections: Int,
    allowPrivateAddresses: Boolean,
    maxMessageSize: Int,
    maxStreamMessageSize: Long,
    packetChunkSize: Int,
    messageConsumers: Int,
    faultToleranceThreshold: Float
)

final case class GrpcServer(
    host: String,
    portExternal: Int,
    portInternal: Int,
    maxMessageSize: Int
)

final case class Tls(
    certificate: Path,
    key: Path,
    customCertificateLocation: Boolean,
    customKeyLocation: Boolean,
    secureRandomNonBlocking: Boolean
)

final case class Kamon(
    prometheus: Boolean,
    influxDb: Boolean,
    influxDbUdp: Boolean,
    zipkin: Boolean,
    sigar: Boolean
)

sealed trait Command
final case class Eval(files: List[String]) extends Command
final case object Repl                     extends Command
final case class Deploy(
    phloLimit: Long,
    phloPrice: Long,
    validAfterBlock: Long,
    privateKey: Option[PrivateKey],
    privateKeyPath: Option[Path],
    location: String
) extends Command
final case class FindDeploy(id: Array[Byte])                               extends Command
final case object Propose                                                  extends Command
final case class ShowBlock(hash: String)                                   extends Command
final case class ShowBlocks(depth: Int)                                    extends Command
final case class VisualizeDag(depth: Int, showJustificationLines: Boolean) extends Command
final case object MachineVerifiableDag                                     extends Command
final case object Run                                                      extends Command
final case class Keygen(algorithm: String, privateKeyPath: Path)           extends Command
final case object LastFinalizedBlock                                       extends Command
final case object Help                                                     extends Command
final case class DataAtName(name: Name)                                    extends Command
final case class ContAtName(names: List[Name])                             extends Command
