package coop.rchain.node

import java.net.{InetAddress, NetworkInterface}
import java.nio.file.{Files, Path, Paths}

import com.typesafe.scalalogging.Logger
import coop.rchain.comm.UPnP
import org.rogach.scallop._

import scala.collection.JavaConverters._

final case class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  version(s"RChain Node ${BuildInfo.version}")

  val name =
    opt[String](default = None, short = 'n', descr = "Node name or key.")

  val port =
    opt[Int](default = Some(30304), short = 'p', descr = "Network port to use.")

  val httpPort =
    opt[Int](default = Some(8080), short = 'x', descr = "HTTP port.")

  val grpcPort =
    opt[Int](default = Some(50000), short = 'g', descr = "gRPC port.")

  val grpcHost =
    opt[String](default = Some("localhost"),
                descr = "Hostname or IP of node you try to connect to via gRPC.")

  val bootstrap =
    opt[String](default = Some("rnode://0f365f1016a54747b384b386b8e85352@216.83.154.106:30012"),
                short = 'b',
                descr = "Bootstrap rnode address for initial seed.")

  val standalone = opt[Boolean](default = Some(false),
                                short = 's',
                                descr = "Start a stand-alone node (no bootstrapping).")

  val host = opt[String](default = None, descr = "Hostname or IP of this node.")

  val repl = opt[Boolean](default = Some(false),
                          short = 'r',
                          descr = "Start node with REPL (but without P2P network)")

  val diagnostics = opt[Boolean](default = Some(false), short = 'd', descr = "Node diagnostics")

  val deployDemo = opt[Boolean](
    default = Some(false),
    descr = "Demo sending some placeholder Deploy operations to Casper at regular intervals")

  val eval = opt[String](default = None,
                         descr = "Start node to evaluate rholang in file (but without P2P network)")

  val data_dir = opt[Path](required = false,
                           descr = "Path to data directory",
                           default = Some(Paths.get("/var/lib/rnode")))

  val map_size = opt[Long](required = false,
                           descr = "Map size (in bytes)",
                           default = Some(1024L * 1024L * 1024L))

  def fetchHost(): String =
    host.toOption match {
      case Some(host) => host
      case None       => whoami(port()).fold("localhost")(_.getHostAddress)
    }

  private def whoami(port: Int): Option[InetAddress] = {

    val logger = Logger("conf")
    val upnp   = new UPnP(port)

    logger.info(s"uPnP: ${upnp.localAddress} -> ${upnp.externalAddress}")

    upnp.localAddress match {
      case Some(addy) => Some(addy)
      case None => {
        val ifaces = NetworkInterface.getNetworkInterfaces.asScala.map(_.getInterfaceAddresses)
        val addresses = ifaces
          .flatMap(_.asScala)
          .map(_.getAddress)
          .toList
          .groupBy(x => x.isLoopbackAddress || x.isLinkLocalAddress || x.isSiteLocalAddress)
        if (addresses.contains(false)) {
          Some(addresses(false).head)
        } else {
          val locals = addresses(true).groupBy(x => x.isLoopbackAddress || x.isLinkLocalAddress)
          if (locals.contains(false)) {
            Some(locals(false).head)
          } else if (locals.contains(true)) {
            Some(locals(true).head)
          } else {
            None
          }
        }
      }
    }
  }

  verify()
}
