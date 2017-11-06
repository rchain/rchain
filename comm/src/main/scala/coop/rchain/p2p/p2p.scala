package coop.rchain.p2p

import coop.rchain.comm._
import com.netaporter.uri.Uri
import coop.rchain.comm.protocol.rchain._
import coop.rchain.comm.protocol.routing

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

  private def handleHandshake(sender: PeerNode, handshake: HandshakeMessage): Unit =
    for {
      resp <- handshake.response(net.local)
    } {
      println(s"Sending handshake response: $resp")
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
              case Protocol.Message.Handshake(hs) =>
                handleHandshake(sender, HandshakeMessage(proto, System.currentTimeMillis))
              case _ => println(s"Unrecognized msg ${upstream}")
            }
          }
        case _ => println(s"Unrecognized message $msg")
      }
    }
}

object NetworkProtocol {
  def handshake(src: ProtocolNode): routing.Protocol = {
    val hs = Protocol().withHandshake(Handshake())
    val packed = com.google.protobuf.any.Any.pack(hs)
    ProtocolMessage.upstreamMessage(src, packed)
  }

  def handshakeResponse(src: ProtocolNode, h: routing.Header): routing.Protocol =
    ProtocolMessage.upstreamResponse(src, h, com.google.protobuf.any.Any.pack(HandshakeResponse()))
}

case class HandshakeMessage(proto: routing.Protocol, timestamp: Long) extends ProtocolMessage {
  def response(src: ProtocolNode): Option[ProtocolMessage] =
    for {
      h <- header
    } yield
      HandshakeResponseMessage(NetworkProtocol.handshakeResponse(src, h),
                               System.currentTimeMillis)
}
case class HandshakeResponseMessage(proto: routing.Protocol, timestamp: Long)
    extends ProtocolResponse
