package coop.rchain.p2p

import coop.rchain.comm._
import com.netaporter.uri.Uri
import coop.rchain.comm.protocol.rchain._
import coop.rchain.comm.protocol.routing
import scala.util.control.NonFatal
import com.google.protobuf.any.{Any => AnyProto}
import com.typesafe.scalalogging.Logger

sealed trait NetworkError
case class ParseError(msg: String) extends NetworkError

/*
 * Inspiration from ethereum:
 *
 *   enode://92c9722c6cd441b9d64cc3f365cdd6cda822c7496b8131078a7362b576526092b58531aa2e5914c4e87e0e574bca4497e68f35a7b66c153ee73b6c7dfc958a34@10.0.0.3:30303
 *
 * =>
 *
 *   rnode://<key>@<host>:<udp-port>
 */
case class NetworkAddress(scheme: String, key: String, host: String, port: Int)

case object NetworkAddress {

  /**
    * Parse a string address into a [[coop.rchain.comm.PeerNode]] or
    * an error indicating that the string could not be parsed into a
    * node address.
    */
  def parse(str: String): Either[ParseError, PeerNode] =
    try {
      val uri = Uri.parse(str)

      val addy =
        for {
          scheme <- uri.scheme
          key <- uri.user
          host <- uri.host
          port <- uri.port
        } yield NetworkAddress(scheme, key, host, port)

      addy match {
        case Some(NetworkAddress(scheme, key, host, port)) =>
          Right(PeerNode(NodeIdentifier(key.getBytes), Endpoint(host, port, port)))
        case _ => Left(ParseError(s"bad address: $str"))
      }
    } catch {
      case NonFatal(e) => Left(ParseError(s"bad address: $str"))
    }
}

case class Network(homeAddress: String) extends ProtocolDispatcher[java.net.SocketAddress] {
  val logger = Logger("p2p")

  val local = NetworkAddress.parse(homeAddress) match {
    case Right(node)           => node
    case Left(ParseError(msg)) => throw new Exception(msg)
  }

  val net = new UnicastNetwork(local, Some(this))

  /**
    * Connect to a remote node named by `remoteAddress`.
    *
    * This method (eventually) will initiate the two-part RChain handshake protocol. First, encryption keys are exchanged,
    * allowing encryption for all future messages. Next protocols are agreed on to ensure that these two nodes can speak
    * the same language.
    */
  def connect(remoteAddress: String): Unit =
    for {
      peer <- NetworkAddress.parse(remoteAddress)
    } {
      logger.debug(s"connect(): Connecting to $peer")
      val ehs = EncryptionHandshakeMessage(NetworkProtocol.encryptionHandshake(net.local),
                                           System.currentTimeMillis)
      val remote = new ProtocolNode(peer, this.net)
      net.roundTrip(ehs, remote) match {
        case Right(resp) => {
          logger.debug(
            s"connect(): Received encryption handshake response from ${resp.sender.get}.")
          val phs = ProtocolHandshakeMessage(NetworkProtocol.protocolHandshake(net.local),
                                             System.currentTimeMillis)
          net.roundTrip(phs, remote) match {
            case Right(resp) => {
              logger.debug(
                s"connect(): Received protocol handshake response from ${resp.sender.get}.")
              net.add(remote)
            }
            case Left(ex) => logger.warn(s"connect(): No phs response: $ex")
          }
        }
        case Left(ex) => logger.warn(s"connect(): No ehs response: $ex")
      }
    }

  def connect(node: PeerNode): Unit =
    connect(node.toAddress)

  def disconnect(): Unit = {
    net.broadcast(
      DisconnectMessage(ProtocolMessage.disconnect(net.local), System.currentTimeMillis))
    ()
  }

  private def handleEncryptionHandshake(sender: PeerNode,
                                        handshake: EncryptionHandshakeMessage): Unit =
    for {
      resp <- handshake.response(net.local)
    } {
      net.comm.send(resp.toByteSeq, sender) match {
        case Right(_) =>
          logger.info(s"Responded to encryption handshake request from $sender.")
        case Left(ex) =>
          logger.error(s"handleEncryptionHandshake(): $ex")
      }
    }

  private def handleProtocolHandshake(sender: PeerNode,
                                      handshake: ProtocolHandshakeMessage): Unit =
    for {
      resp <- handshake.response(net.local)
    } {
      net.comm.send(resp.toByteSeq, sender) match {
        case Right(_) =>
          logger.info(s"Responded to protocol handshake request from $sender.")
        case Left(ex) =>
          logger.error(s"handleProtocolHandshake(): $ex")
      }
      // TODO: This add() call should be conditional on actually going
      // through the handshake process, but for now it helps
      // demonstrate network-building.
      net.add(sender)
    }

  override def dispatch(sock: java.net.SocketAddress, msg: ProtocolMessage): Unit =
    for {
      sndr <- msg.sender
    } {
      val sender =
        sock match {
          case (s: java.net.InetSocketAddress) =>
            sndr.withUdpSocket(s)
          case _ => sndr
        }

      msg match {
        case upstream @ UpstreamMessage(proto, _) =>
          for {
            msg <- proto.message.upstream
          } {
            msg.typeUrl match {
              case "type.googleapis.com/coop.rchain.comm.protocol.rchain.EncryptionHandshake" =>
                handleEncryptionHandshake(
                  sender,
                  EncryptionHandshakeMessage(proto, System.currentTimeMillis))
              case "type.googleapis.com/coop.rchain.comm.protocol.rchain.ProtocolHandshake" =>
                handleProtocolHandshake(sender,
                                        ProtocolHandshakeMessage(proto, System.currentTimeMillis))
              case _ => logger.warn(s"Unexpected message type ${msg.typeUrl}")
            }
          }
        /*
         * We do not expect to get any responses to the protocol out
         * of the blue; rather, they'll all be done through
         * synchronous, request-response messaging.
         */
        case upstream @ UpstreamResponse(proto, _) =>
          logger.error(s"Out-of-sequence message: $upstream")
        case _ => logger.warn(s"Unrecognized msg ${msg}")
      }
    }

  override def toString = s"#{Network $homeAddress}"
}

object NetworkProtocol {
  def encryptionHandshake(src: ProtocolNode): routing.Protocol =
    ProtocolMessage.upstreamMessage(src, AnyProto.pack(EncryptionHandshake()))

  def encryptionHandshakeResponse(src: ProtocolNode, h: routing.Header): routing.Protocol =
    ProtocolMessage.upstreamResponse(src, h, AnyProto.pack(EncryptionHandshakeResponse()))

  def protocolHandshake(src: ProtocolNode): routing.Protocol =
    ProtocolMessage.upstreamMessage(src, AnyProto.pack(ProtocolHandshake()))

  def protocolHandshakeResponse(src: ProtocolNode, h: routing.Header): routing.Protocol =
    ProtocolMessage.upstreamResponse(src, h, AnyProto.pack(ProtocolHandshakeResponse()))
}

case class EncryptionHandshakeMessage(proto: routing.Protocol, timestamp: Long)
    extends ProtocolMessage {
  def response(src: ProtocolNode): Option[ProtocolMessage] =
    for {
      h <- header
    } yield
      EncryptionHandshakeResponseMessage(NetworkProtocol.encryptionHandshakeResponse(src, h),
                                         System.currentTimeMillis)
}
case class EncryptionHandshakeResponseMessage(proto: routing.Protocol, timestamp: Long)
    extends ProtocolResponse

case class ProtocolHandshakeMessage(proto: routing.Protocol, timestamp: Long)
    extends ProtocolMessage {
  def response(src: ProtocolNode): Option[ProtocolMessage] =
    for {
      h <- header
    } yield
      ProtocolHandshakeResponseMessage(NetworkProtocol.protocolHandshakeResponse(src, h),
                                       System.currentTimeMillis)
}
case class ProtocolHandshakeResponseMessage(proto: routing.Protocol, timestamp: Long)
    extends ProtocolResponse
