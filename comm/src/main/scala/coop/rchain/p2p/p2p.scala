package coop.rchain.p2p

import coop.rchain.comm._
import com.netaporter.uri.Uri
import coop.rchain.comm.protocol.rchain._
import coop.rchain.comm.protocol.routing
import scala.util.control.NonFatal
import com.google.protobuf.any.{Any => AnyProto}

sealed trait NetworkError
case class ParseError(msg: String) extends NetworkError

/*
 * Inspiration from ethereum:
 *
 *   enode://92c9722c6cd441b9d64cc3f365cdd6cda822c7496b8131078a7362b576526092b58531aa2e5914c4e87e0e574bca4497e68f35a7b66c153ee73b6c7dfc958a34@10.0.0.3:30303
 *
 * =>
 *
 *   rnode://<key>@<ip>:<udp-port>
 */
case class NetworkAddress(scheme: String, key: String, host: String, port: Int)

case object NetworkAddress {
  def parse(str: String): Either[ParseError, PeerNode] = {
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
        case Some(NetworkAddress("rnode", key, host, port)) =>
          Right(PeerNode(NodeIdentifier(key.getBytes), Endpoint(host, port, port)))
        case _ => Left(ParseError(s"bad address: $str"))
      }
    } catch {
      case NonFatal(e) => Left(ParseError(s"bad address: $str"))
    }
  }
}

case class Network(homeAddress: String) extends ProtocolDispatcher {
  val local = NetworkAddress.parse(homeAddress) match {
    case Right(node)           => node
    case Left(ParseError(msg)) => throw new Exception(msg)
  }

  val net = new UnicastNetwork(local, Some(this))

  def dial(remoteAddress: String): Unit =
    for {
      peer <- NetworkAddress.parse(remoteAddress)
    } {
      // TODO: Initiate Handshake with peer.
      println(peer)
    }

  private def handleEncryptionHandshake(sender: PeerNode, handshake: EncryptionHandshakeMessage): Unit =
    for {
      resp <- handshake.response(net.local)
    } {
      println(s"Sending enc handshake response: $resp")
      net.comm.send(resp.toByteSeq, sender)
    }

  private def handleProtocolHandshake(sender: PeerNode, handshake: ProtocolHandshakeMessage): Unit =
    for {
      resp <- handshake.response(net.local)
    } {
      println(s"Sending enc handshake response: $resp")
      net.comm.send(resp.toByteSeq, sender)
    }

  override def dispatch(msg: ProtocolMessage): Unit =
    for {
      sender <- msg.sender
    } {
      println(s"Got a thing: $msg")
      msg match {
        case upstream @ UpstreamMessage(proto, _) =>
          for {
            upstream <- proto.message.upstream
          } {
            upstream.unpack(Protocol).message match {
              case Protocol.Message.EncryptionHandshake(hs) =>
                handleEncryptionHandshake(sender, EncryptionHandshakeMessage(proto, System.currentTimeMillis))
              case Protocol.Message.ProtocolHandshake(hs) =>
                handleProtocolHandshake(sender, ProtocolHandshakeMessage(proto, System.currentTimeMillis))
              case _ => println(s"Unrecognized msg ${upstream}")
            }
          }
        case _ => println(s"Unrecognized message $msg")
      }
    }
}

object NetworkProtocol {
  def encryptionHandshake(src: ProtocolNode): routing.Protocol = {
    val hs = Protocol().withEncryptionHandshake(EncryptionHandshake())
    val packed = AnyProto.pack(hs)
    ProtocolMessage.upstreamMessage(src, packed)
  }

  def encryptionHandshakeResponse(src: ProtocolNode, h: routing.Header): routing.Protocol =
    ProtocolMessage.upstreamResponse(src, h, AnyProto.pack(EncryptionHandshakeResponse()))

  def protocolHandshake(src: ProtocolNode): routing.Protocol = {
    val hs = Protocol().withProtocolHandshake(ProtocolHandshake())
    val packed = AnyProto.pack(hs)
    ProtocolMessage.upstreamMessage(src, packed)
  }

  def protocolHandshakeResponse(src: ProtocolNode, h: routing.Header): routing.Protocol =
    ProtocolMessage.upstreamResponse(src, h, AnyProto.pack(ProtocolHandshakeResponse()))
}

case class EncryptionHandshakeMessage(proto: routing.Protocol, timestamp: Long) extends ProtocolMessage {
  def response(src: ProtocolNode): Option[ProtocolMessage] =
    for {
      h <- header
    } yield
      EncryptionHandshakeResponseMessage(NetworkProtocol.encryptionHandshakeResponse(src, h),
                               System.currentTimeMillis)
}
case class EncryptionHandshakeResponseMessage(proto: routing.Protocol, timestamp: Long)
    extends ProtocolResponse

case class ProtocolHandshakeMessage(proto: routing.Protocol, timestamp: Long) extends ProtocolMessage {
  def response(src: ProtocolNode): Option[ProtocolMessage] =
    for {
      h <- header
    } yield
      ProtocolHandshakeResponseMessage(NetworkProtocol.protocolHandshakeResponse(src, h),
                               System.currentTimeMillis)
}
case class ProtocolHandshakeResponseMessage(proto: routing.Protocol, timestamp: Long)
    extends ProtocolResponse
