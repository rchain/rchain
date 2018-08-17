package coop.rchain.node.configuration

import java.nio.file.Path

import coop.rchain.comm.PeerNode

case class Server(
    host: Option[String],
    port: Int,
    httpPort: Int,
    metricsPort: Int,
    noUpnp: Boolean,
    defaultTimeout: Int,
    bootstrap: PeerNode,
    standalone: Boolean,
    dataDir: Path,
    mapSize: Long,
    inMemoryStore: Boolean,
    maxNumOfConnections: Int
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
case class Deploy(address: String, phlo: Int, phloPrice: Int, nonce: Int, location: String)
    extends Command
case object DeployDemo             extends Command
case object Propose                extends Command
case class ShowBlock(hash: String) extends Command
case object ShowBlocks             extends Command
case object Run                    extends Command
case object Help                   extends Command
