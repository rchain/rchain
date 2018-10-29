package coop.rchain.node.configuration

import java.nio.file.Path

import coop.rchain.casper.util.comm.ListenAtName.Name
import coop.rchain.comm.PeerNode
import coop.rchain.shared.StoreType

case class Server(
    host: Option[String],
    port: Int,
    httpPort: Int,
    kademliaPort: Int,
    dynamicHostAddress: Boolean,
    noUpnp: Boolean,
    defaultTimeout: Int,
    bootstrap: PeerNode,
    standalone: Boolean,
    genesisValidator: Boolean,
    dataDir: Path,
    mapSize: Long,
    storeType: StoreType,
    maxNumOfConnections: Int,
    maxMessageSize: Int
)

case class GrpcServer(
    host: String,
    portExternal: Int,
    portInternal: Int
)

case class Tls(
    certificate: Path,
    key: Path,
    customCertificateLocation: Boolean,
    customKeyLocation: Boolean,
    secureRandomNonBlocking: Boolean
)

sealed trait Command
case class Eval(files: List[String]) extends Command
case object Repl                     extends Command
case object Diagnostics              extends Command
case class Deploy(
    address: String,
    phloLimit: Long,
    phloPrice: Long,
    nonce: Int,
    location: String
) extends Command
case object DeployDemo                   extends Command
case object Propose                      extends Command
case class ShowBlock(hash: String)       extends Command
case class ShowBlocks(depth: Int)        extends Command
case object Run                          extends Command
case object Help                         extends Command
case class DataAtName(name: Name)        extends Command
case class ContAtName(names: List[Name]) extends Command
case class BondingDeployGen(
    bondKey: String,
    ethAddress: String,
    amount: Long,
    secKey: String,
    pubKey: String
) extends Command
case class FaucetBondingDeployGen(
    amount: Long,
    sigAlgorithm: String,
    secKey: String,
    pubKey: String
) extends Command
