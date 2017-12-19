package coop.rchain.comm

import java.net.InetAddress

import org.bitlet.weupnp.{GatewayDevice, GatewayDiscover}
// import scorex.core.settings.NetworkSettings
// import scorex.core.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.util.Try
import com.typesafe.scalalogging.Logger

class UPnP// (settings: NetworkSettings) extends ScorexLogging
{
  val logger = Logger("upnp")

  private var gateway: Option[GatewayDevice] = None

  lazy val localAddress = gateway.map(_.getLocalAddress)
  lazy val externalAddress = gateway.map(_.getExternalIPAddress).map(InetAddress.getByName)

  Try {
    logger.info("Looking for UPnP gateway device...")
    val defaultHttpReadTimeout = 5000 // settings.upnpGatewayTimeout.map(_.toMillis.toInt).getOrElse(GatewayDevice.getHttpReadTimeout)
    GatewayDevice.setHttpReadTimeout(defaultHttpReadTimeout)
    val discover = new GatewayDiscover()
    val defaultDiscoverTimeout = 5000 // settings.upnpDiscoverTimeout.map(_.toMillis.toInt).getOrElse(discover.getTimeout)
    discover.setTimeout(defaultDiscoverTimeout)

    logger.info(s"${discover.discover}")

    val gatewayMap = Option(discover.discover).map(_.asScala).map(_.toMap).getOrElse(Map())
    if (gatewayMap.isEmpty) {
      logger.debug("There are no UPnP gateway devices")
    } else {
      gatewayMap.foreach { case (addr, _) =>
        logger.debug("UPnP gateway device found on " + addr.getHostAddress)
      }
      Option(discover.getValidGateway) match {
        case None => logger.debug("There is no connected UPnP gateway device")
        case Some(device) =>
          gateway = Some(device)
          logger.debug("Using UPnP gateway device on " + localAddress.map(_.getHostAddress).getOrElse("err"))
          logger.info("External IP address is " + externalAddress.map(_.getHostAddress).getOrElse("err"))
      }
    }
  }.recover { case t: Throwable =>
    logger.error("Unable to discover UPnP gateway devices: " + t.toString)
  }

  addPort(30304)
  // if (settings.upnpEnabled) addPort(settings.port)

  def addPort(port: Int): Try[Unit] = Try {
    if (gateway.get.addPortMapping(port, port, localAddress.get.getHostAddress, "UDP", "RChain")) {
      logger.debug("Mapped port [" + externalAddress.get.getHostAddress + "]:" + port)
    } else {
      logger.debug("Unable to map port " + port)
    }
  }.recover { case t: Throwable =>
    logger.error("Unable to map port " + port + ": " + t.toString)
  }

  def deletePort(port: Int): Try[Unit] = Try {
    if (gateway.get.deletePortMapping(port, "TCP")) {
      logger.debug("Mapping deleted for port " + port)
    } else {
      logger.debug("Unable to delete mapping for port " + port)
    }
  }.recover { case t: Throwable =>
    logger.error("Unable to delete mapping for port " + port + ": " + t.toString)
  }
}
