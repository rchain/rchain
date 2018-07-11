package coop.rchain.comm

import java.net.InetAddress

import scala.collection.JavaConverters._
import scala.util.Try
import scala.util.control.NonFatal
import scala.util.matching.Regex

import org.bitlet.weupnp._

object UPnP {

  lazy val IPv4: Regex =
    ("^(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\." +
      "(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\." +
      "(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\." +
      "(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$").r

  def isPrivateIpAddress(ip: String): Option[Boolean] = {
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

  def assurePortForwarding(ports: Seq[Int]): Option[String] = {
    println("INFO - trying to open ports using UPnP....")
    val devices = discover
    if (devices.gateways.isEmpty) {
      println("INFO - No gateway devices found")
      if (devices.all.isEmpty) {
        println("INFO - No need to open any port")
      } else {
        println("INFO - Other devices:")
        devices.all.foreach {
          case (ip, d) =>
            println(UPnP.showDevice(ip, d))
        }
        println()
      }
      None
    } else {
      print("INFO - Available gateway devices: ")
      println(devices.gateways.map(d => d.getFriendlyName).mkString(", "))

      val gateway = devices.validGateway.getOrElse(devices.gateways.head)
      println(s"INFO - Picking ${gateway.getFriendlyName} as gateway")
      isPrivateIpAddress(gateway.getExternalIPAddress) match {
        case Some(true) =>
          println(
            s"WARNING - Gateway's external IP address ${gateway.getExternalIPAddress} is from a private address block. " +
              "This machine is behind more than one NAT.")
        case Some(_) =>
          println("INFO - Gateway's external IP address is from a public address block.")
        case _ => println("WARNING - Can't parse gateway's external IP address. It's maybe IPv6.")
      }

      val mappings = getPortMappings(gateway).filter(m => ports.contains(m.getExternalPort))
      mappings.foreach { m =>
        print(
          s"INFO - Removing an existing port mapping for port ${m.getProtocol}/${m.getExternalPort}")
        removePort(gateway, m) match {
          case Right(_) => println(" [success]")
          case _        => println(" [failed]")
        }
      }

      val result = ports.map { p =>
        print(s"INFO - Adding a port mapping for port TCP/$p")
        addPort(gateway, p, "TCP", "RChain") match {
          case Right(_) =>
            println(" [success]")
            true
          case _ =>
            println(" [failed]")
            false
        }
      }

      if (result.exists(r => !r)) {
        println(
          "ERROR - Could not open the ports via UPnP. Please open it manually on your router!")
      } else {
        println("INFO - UPnP port forwarding was most likely successful!")
      }
      println()
      println(showPortMappingHeader)
      getPortMappings(gateway).foreach(m => println(showPortMapping(m)))
      println()
      Some(gateway.getExternalIPAddress)
    }
  }

  def discover: UPnPDevices = {
    val discover = new GatewayDiscover
    UPnPDevices(
      discover.discover.asScala.toMap,
      discover.getAllGateways.asScala.toMap.values.toSeq,
      Option(discover.getValidGateway)
    )
  }

  def getPortMappings(device: GatewayDevice): Seq[PortMappingEntry] = {

    def loop(i: Int, mappings: Seq[PortMappingEntry]): Seq[PortMappingEntry] = {
      val entry = new PortMappingEntry
      if (device.getGenericPortMappingEntry(i, entry)) {
        loop(i + 1, entry +: mappings)
      } else mappings.reverse
    }

    loop(0, Seq.empty)
  }

  def showDevice(ip: InetAddress, device: GatewayDevice): String =
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

  val showPortMappingHeader: String = {
    val externalPort   = colExternalPort.format("Extern")
    val internalPort   = colInternalPort.format("Intern")
    val internalClient = colInternalClient.format("Host")
    val protocol       = colProtocol.format("Protocol")
    val description    = "Description"
    s"$protocol $externalPort $internalClient $internalPort $description"
  }

  def showPortMapping(m: PortMappingEntry): String = {
    val externalPort   = colExternalPort.format(s"${m.getExternalPort}")
    val internalPort   = colInternalPort.format(s"${m.getInternalPort}")
    val internalClient = colInternalClient.format(m.getInternalClient)
    val protocol       = colProtocol.format(m.getProtocol)
    val description    = m.getPortMappingDescription
    s"$protocol $externalPort $internalClient $internalPort $description"
  }

  // TODO: Allow different external and internal ports
  def addPort(device: GatewayDevice,
              port: Int,
              protocol: String,
              description: String): Either[CommError, Boolean] =
    try {
      Right(
        device
          .addPortMapping(port, port, device.getLocalAddress.getHostAddress, protocol, description))
    } catch {
      case NonFatal(ex: Exception) => Left(UnknownCommError(ex.toString))
    }

  def removePort(device: GatewayDevice, portMapping: PortMappingEntry): Either[CommError, Unit] =
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
