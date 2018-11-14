package coop.rchain.comm

import java.net.InetAddress

import cats.Monad
import cats.effect.Sync
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import coop.rchain.shared.Log
import org.bitlet.weupnp._

import scala.collection.JavaConverters._
import scala.language.higherKinds
import scala.util.Try
import scala.util.control.NonFatal
import scala.util.matching.Regex

object UPnP {

  lazy val IPv4: Regex =
    ("^(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\." +
      "(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\." +
      "(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\." +
      "(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$").r

  private def isPrivateIpAddress(ip: String): Option[Boolean] = {
    val parts =
      ip match {
        case IPv4(p1, p2, p3, p4) => Some((p1.toInt, p2.toInt, p3.toInt, p4.toInt))
        case _                    => None
      }

    parts.map {
      case (10, _, _, _)                      => true
      case (127, _, _, _)                     => true
      case (192, 168, _, _)                   => true
      case (172, p, _, _) if p > 15 && p < 32 => true
      case (169, 254, _, _)                   => true
      case (0, 0, 0, 0)                       => true
      case _                                  => false
    }
  }

  private def printDevices[F[_]: Log](devices: UPnPDevices): F[Unit] = {
    val devicesStr = devices.all
      .map {
        case (ip, d) =>
          UPnP.showDevice(ip, d)
      }
      .mkString("\n", "\n", "\n")

    Log[F].info(s"Other devices: $devicesStr")
  }

  private def logGatewayEmpty[F[_]: Log: Monad](devices: UPnPDevices): F[Unit] =
    for {
      _ <- Log[F].info("INFO - No gateway devices found")
      _ <- if (devices.all.isEmpty) Log[F].info("No need to open any port")
          else printDevices[F](devices)
    } yield ()

  private def removePorts[F[_]: Log: Sync](
      mappings: List[PortMappingEntry],
      gateway: GatewayDevice
  ): F[List[Unit]] =
    mappings.traverse { m =>
      for {
        res    <- Sync[F].delay(removePort(gateway, m))
        resMsg = if (res.isRight) "[success]" else "[failed]"
        _ <- Log[F].info(
              s"Removing an existing port mapping for port ${m.getProtocol}/${m.getExternalPort} $resMsg"
            )
      } yield ()
    }

  private def addPorts[F[_]: Log: Sync](
      ports: List[Int],
      gateway: GatewayDevice
  ): F[List[Boolean]] =
    ports.traverse { p =>
      for {
        res    <- Sync[F].delay(addPort(gateway, p, "TCP", "RChain"))
        resMsg = if (res.isRight) "[success]" else "[failed]"
        _      <- Log[F].info(s"Adding a port mapping for port TCP/$p $resMsg")
      } yield res.isRight
    }

  private def tryOpenPorts[F[_]: Log: Sync](
      ports: List[Int],
      devices: UPnPDevices
  ): F[Option[String]] =
    for {
      _ <- Log[F].info(
            s"Available gateway devices: ${devices.gateways.map(d => d.getFriendlyName).mkString(", ")}"
          )
      gateway = devices.validGateway.getOrElse(devices.gateways.head)
      _       <- Log[F].info(s"Picking ${gateway.getFriendlyName} as gateway")
      _ <- isPrivateIpAddress(gateway.getExternalIPAddress) match {
            case Some(true) =>
              Log[F].warn(
                s"Gateway's external IP address ${gateway.getExternalIPAddress} is from a private address block. " +
                  "This machine is behind more than one NAT."
              )
            case Some(_) =>
              Log[F].info("Gateway's external IP address is from a public address block.")
            case _ =>
              Log[F].warn("Can't parse gateway's external IP address. It's maybe IPv6.")
          }

      mappings <- Sync[F].delay(
                   getPortMappings(gateway).filter(m => ports.contains(m.getExternalPort)).toList
                 )
      _   <- removePorts(mappings, gateway)
      res <- addPorts(ports, gateway)

      _ <- if (res.exists(r => !r))
            Log[F].error(
              "Could not open the ports via UPnP. Please open it manually on your router!"
            )
          else
            Log[F].info("UPnP port forwarding was most likely successful!")

      _ <- Log[F].info(showPortMappingHeader)
      _ <- getPortMappings(gateway).toList.map(showPortMapping).traverse(Log[F].info)
    } yield Some(gateway.getExternalIPAddress)

  def assurePortForwarding[F[_]: Log: Sync](ports: List[Int]): F[Option[String]] =
    for {
      _       <- Log[F].info("trying to open ports using UPnP....")
      devices <- Sync[F].delay(discover)
      res <- if (devices.gateways.isEmpty) logGatewayEmpty(devices) *> None.pure[F]
            else tryOpenPorts(ports, devices)
    } yield res

  private def discover: UPnPDevices = {
    val discover = new GatewayDiscover
    UPnPDevices(
      discover.discover.asScala.toMap,
      discover.getAllGateways.asScala.toMap.values.toSeq,
      Option(discover.getValidGateway)
    )
  }

  private def getPortMappings(device: GatewayDevice): Seq[PortMappingEntry] = {

    def loop(i: Int, mappings: Seq[PortMappingEntry]): Seq[PortMappingEntry] = {
      val entry = new PortMappingEntry
      if (device.getGenericPortMappingEntry(i, entry)) {
        loop(i + 1, entry +: mappings)
      } else mappings.reverse
    }

    loop(0, Seq.empty)
  }

  private def showDevice(ip: InetAddress, device: GatewayDevice): String =
    s"""
       |Interface:    ${ip.getHostAddress}
       |Name:         ${device.getFriendlyName}
       |Model:        ${device.getModelName}
       |Manufacturer: ${device.getManufacturer}
       |Description:  ${device.getModelDescription}
       |Type:         ${device.getDeviceType}
       |Search type:  ${device.getSt}
       |Service type: ${device.getServiceType}
       |Location:     ${device.getLocation}
       |External IP:  ${device.getExternalIPAddress}
       |Connected:    ${Try(device.isConnected).filter(identity).map(_ => "yes").getOrElse("no")}
       |""".stripMargin

  private val colExternalPort   = "%1$-8s"
  private val colInternalPort   = "%1$-8s"
  private val colInternalClient = "%1$-15s"
  private val colProtocol       = "%1$-10s"

  private val showPortMappingHeader: String = {
    val externalPort   = colExternalPort.format("Extern")
    val internalPort   = colInternalPort.format("Intern")
    val internalClient = colInternalClient.format("Host")
    val protocol       = colProtocol.format("Protocol")
    val description    = "Description"
    s"$protocol $externalPort $internalClient $internalPort $description"
  }

  private def showPortMapping(m: PortMappingEntry): String = {
    val externalPort   = colExternalPort.format(s"${m.getExternalPort}")
    val internalPort   = colInternalPort.format(s"${m.getInternalPort}")
    val internalClient = colInternalClient.format(m.getInternalClient)
    val protocol       = colProtocol.format(m.getProtocol)
    val description    = m.getPortMappingDescription
    s"$protocol $externalPort $internalClient $internalPort $description"
  }

  // TODO: Allow different external and internal ports
  private def addPort(
      device: GatewayDevice,
      port: Int,
      protocol: String,
      description: String
  ): Either[CommError, Boolean] =
    try {
      Right(
        device
          .addPortMapping(port, port, device.getLocalAddress.getHostAddress, protocol, description)
      )
    } catch {
      case NonFatal(ex: Exception) => Left(UnknownCommError(ex.toString))
    }

  private def removePort(
      device: GatewayDevice,
      portMapping: PortMappingEntry
  ): Either[CommError, Unit] =
    try {
      device.deletePortMapping(portMapping.getExternalPort, portMapping.getProtocol)
      Right(())
    } catch {
      case NonFatal(ex: Exception) => Left(UnknownCommError(ex.toString))
    }
}

case class UPnPDevices(
    all: Map[InetAddress, GatewayDevice],
    gateways: Seq[GatewayDevice],
    validGateway: Option[GatewayDevice]
)
