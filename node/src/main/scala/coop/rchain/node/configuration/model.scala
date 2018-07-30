package coop.rchain.node.configuration

import java.nio.file.Path

import coop.rchain.blockstorage.LMDBBlockStore
import coop.rchain.casper.CasperConf
import coop.rchain.comm.PeerNode

abstract class Configuration(
    val command: Command,
    val server: Server,
    val grpcServer: GrpcServer,
    val tls: Tls,
    val casper: CasperConf,
    val blockstorage: LMDBBlockStore.Config
) {
  def printHelp(): Unit
  def fetchHost(externalAddress: Option[String]): String
}

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
    mapSize: Long
)

case class GrpcServer(
    host: String,
    port: Int
)

case class Tls(
    certificate: Path,
    key: Path,
    customCertificateLocation: Boolean,
    customKeyLocation: Boolean
)

sealed trait Command
case class Eval(files: List[String]) extends Command
case object Repl                     extends Command
case object Diagnostics              extends Command
case class Deploy(location: String)  extends Command
case object DeployDemo               extends Command
case object Propose                  extends Command
case class ShowBlock(hash: String)   extends Command
case object ShowBlocks               extends Command
case object Run                      extends Command
case object Help                     extends Command
