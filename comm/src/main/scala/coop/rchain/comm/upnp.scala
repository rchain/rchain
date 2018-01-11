package coop.rchain.comm

import java.net.InetAddress

import org.bitlet.weupnp.{GatewayDevice, GatewayDiscover}

import scala.util.control.NonFatal
import scala.collection.JavaConverters._
import scala.util.Try
import com.typesafe.scalalogging.Logger

/**
  * Simple class that attempts to find and use a uPnP router for networking.
  *
  * Registers the given port for UDP transmission through a NAT.
  *
  * Cribbed from https://github.com/ScorexFoundation/Scorex/blob/503fda982d96707db8731f954ac6428b1d17e4e8/src/main/scala/scorex/core/network/UPnP.scala.
  */
class UPnP(port: Int) {
  val logger = Logger("upnp")

  val gateway: Option[GatewayDevice] = {
    GatewayDevice.setHttpReadTimeout(5000)
    val discover = new GatewayDiscover
    discover.setTimeout(5000)

    discover.discover

    Option(discover.getValidGateway)
  }

  def localAddress: Option[InetAddress] = gateway.map(_.getLocalAddress)
  def externalAddress: Option[String] = gateway.map(_.getExternalIPAddress)

  addPort(port)

  def addPort(port: Int): Either[CommError, Boolean] =
    try {
      gateway match {
        case Some(device) =>
          Right(
            device.addPortMapping(port, port, localAddress.get.getHostAddress, "UDP", "RChain"))
        case None => Left(UnknownCommError("no gateway"))
      }
    } catch {
      case NonFatal(ex: Exception) => Left(UnknownCommError(ex.toString))
    }

  def removePort(port: Int): Either[CommError, Unit] =
    try {
      gateway match {
        case Some(device) =>
          device.deletePortMapping(port, "UDP")
          Right(())
        case None => Left(UnknownCommError("no gateway"))
      }
    } catch {
      case NonFatal(ex: Exception) => Left(UnknownCommError(ex.toString))
    }
}
