package coop.rchain.comm

import java.net.InetAddress

import org.bitlet.weupnp.{GatewayDevice, GatewayDiscover}

import scala.util.control.NonFatal
import scala.collection.JavaConverters._
import scala.util.Try

/**
  * Simple class that attempts to find and use a uPnP router for networking.
  *
  * Registers the given port for TCP transmission through a NAT.
  *
  * Cribbed from https://github.com/ScorexFoundation/Scorex/blob/503fda982d96707db8731f954ac6428b1d17e4e8/src/main/scala/scorex/core/network/UPnP.scala.
  */
class UPnP {
  lazy val gateway: Option[GatewayDevice] = {
    GatewayDevice.setHttpReadTimeout(5000)
    val discover = new GatewayDiscover
    discover.setTimeout(5000)
    discover.discover
    Option(discover.getValidGateway)
  }

  def localAddress: Option[InetAddress] = gateway.map(_.getLocalAddress)
  def externalAddress: Option[String]   = gateway.map(_.getExternalIPAddress)

  def addPort(port: Int): Either[CommError, Boolean] =
    try {
      gateway match {
        case Some(device) =>
          Right(device.addPortMapping(port, port, localAddress.get.getHostAddress, "TCP", "RChain"))
        case None => Left(UnknownCommError("no gateway"))
      }
    } catch {
      case NonFatal(ex: Exception) => Left(UnknownCommError(ex.toString))
    }

  def removePort(port: Int): Either[CommError, Unit] =
    try {
      gateway match {
        case Some(device) =>
          device.deletePortMapping(port, "TCP")
          Right(())
        case None => Left(UnknownCommError("no gateway"))
      }
    } catch {
      case NonFatal(ex: Exception) => Left(UnknownCommError(ex.toString))
    }
}
