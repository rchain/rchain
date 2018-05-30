package coop.rchain.node

import java.net.{InetAddress, NetworkInterface}
import java.nio.file.{Path, Paths}

import com.typesafe.scalalogging.Logger
import coop.rchain.comm.UPnP
import org.rogach.scallop._

import scala.collection.JavaConverters._

final case class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  version(s"RChain Node ${BuildInfo.version}")

  val diagnostics = opt[Boolean](default = Some(false), short = 'd', descr = "Node diagnostics")

  val certificate =
    opt[Path](
      required = false,
      short = 'c',
      descr = "Path to node's X.509 certificate file, that is being used for identification")

  val key =
    opt[Path](required = false,
              short = 'k',
              descr =
                "Path to node's private key PEM file, that is being used for TLS communication")

  val port =
    opt[Int](default = Some(30304), short = 'p', descr = "Network port to use.")

  val httpPort =
    opt[Int](default = Some(8080),
             descr = "HTTP port (deprecated - all API features will be ported to gRPC API).")

  val grpcPort =
    opt[Int](default = Some(50000), descr = "Port used for gRPC API.")

  val metricsPort =
    opt[Int](default = Some(9095), descr = "Port used by metrics API.")

  val grpcHost =
    opt[String](default = Some("localhost"),
                descr = "Hostname or IP of node on which gRPC service is running.")

  val numValidators = opt[Int](default = Some(5), descr = "Number of validators at genesis.")
  val bondsFile = opt[String](
    default = None,
    descr = "Plain text file consisting of lines of the form `<pk> <stake>`, " +
      "which defines the bond amounts for each validator at genesis. " +
      "<pk> is the public key (in base-16 encoding) identifying the validator and <stake>" +
      "is the amount of Rev they have bonded (an integer). Note: this overrides the --num-validators option."
  )

  val bootstrap =
    opt[String](default = Some("rnode://acd0b05a971c243817a0cfd469f5d1a238c60294@216.83.154.106:30304"),
                short = 'b',
                descr = "Bootstrap rnode address for initial seed.")

  val standalone = opt[Boolean](default = Some(false),
                                short = 's',
                                descr = "Start a stand-alone node (no bootstrapping).")

  val host = opt[String](default = None, descr = "Hostname or IP of this node.")

  val repl = opt[Boolean](
    default = Some(false),
    short = 'r',
    descr = "Starts a thin client, that will connect to existing node. See grpcHost and grpcPort.")

  val eval = opt[String](
    default = None,
    descr =
      "Starts a thin client that will evaluate rholang in file on a existing running node. See grpcHost and grpcPort.")

  val data_dir = opt[Path](required = false,
                           descr = "Path to data directory. Defaults to /var/lib/rnode",
                           default = Some(Paths.get("/var/lib/rnode")))

  val map_size = opt[Long](required = false,
                           descr = "Map size (in bytes)",
                           default = Some(1024L * 1024L * 1024L))

  val deployDemo = opt[Boolean](
    default = Some(false),
    descr =
      "Demo sending some placeholder Deploy operations to Casper on an existing running node at regular intervals")

  val deploy = opt[String](
    default = None,
    descr =
      "Deploy a Rholang source file to Casper on an existing running node. " +
        "The deploy will be packaged and sent as a block to the network depending " +
        "on the configuration of the Casper instance."
  )

  val showBlock = opt[String](
    default = None,
    descr =
      "View properties of a block known by Casper on an existing running node." +
        "Output includes: parent hashes, storage contents of the tuplespace."
  )

  val propose = opt[Boolean](
    default = Some(false),
    descr =
      "Force Casper (on an existing running node) to propose a block based on its accumulated deploys. " +
        "Requires a value of --secret-key to be set."
  )

  val secretKey = opt[String](
    default = None,
    descr = "Base16 encoding of the Ed25519 private key to use for signing a proposed block.")

  def fetchHost(): String =
    host.toOption match {
      case Some(host) => host
      case None       => whoami(port()).fold("localhost")(_.getHostAddress)
    }

  def certificatePath: Path =
    certificate.toOption
      .getOrElse(Paths.get(data_dir().toString, "node.certificate.pem"))

  def keyPath: Path =
    certificate.toOption
      .getOrElse(Paths.get(data_dir().toString, "node.key.pem"))

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
