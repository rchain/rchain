package coop.rchain.comm

import coop.rchain.kademlia
import coop.rchain.comm.protocol.routing._
import scala.util.control.NonFatal

import com.google.protobuf.any.{Any => AnyProto}
import coop.rchain.catscontrib._
import Catscontrib._

import com.google.protobuf.ByteString

object ProtocolNode {

  def apply(peer: PeerNode): ProtocolNode =
    new ProtocolNode(peer.id, peer.endpoint)
}

class ProtocolNode private (id: NodeIdentifier, endpoint: Endpoint) extends PeerNode(id, endpoint)

/**
  * `ProtocolMessage` insulates protocol handlers from protocol buffer
  * clutter.
  */
trait ProtocolMessage {
  val proto: Protocol
  val timestamp: Long

  def header: Option[Header] = proto.header

  def sender: Option[PeerNode] =
    for {
      h <- header
      s <- h.sender
    } yield
      PeerNode(NodeIdentifier(s.id.toByteArray),
               Endpoint(s.host.toStringUtf8, s.tcpPort, s.udpPort))

  def toByteSeq: Seq[Byte] =
    proto.toByteArray
}

/**
  * A ping is a simple are-you-there? message.
  */
final case class PingMessage(proto: Protocol, timestamp: Long) extends ProtocolMessage {
  def response(src: ProtocolNode): Option[ProtocolMessage] =
    for {
      h <- header
    } yield PongMessage(ProtocolMessage.pong(src), System.currentTimeMillis)
}

/**
  * A pong is the response to a ping.
  */
final case class PongMessage(proto: Protocol, timestamp: Long) extends ProtocolMessage

/**
  * A lookup message asks for a list of peers from the local Kademlia
  * table that are closest to a given key.
  */
final case class LookupMessage(proto: Protocol, timestamp: Long) extends ProtocolMessage {
  def lookupId: Option[Seq[Byte]] = proto.message.lookup.map(_.id.toByteArray)

  def response(src: ProtocolNode, nodes: Seq[PeerNode]): Option[ProtocolMessage] =
    header.map { h =>
      LookupResponseMessage(ProtocolMessage.lookupResponse(src, nodes), System.currentTimeMillis)
    }
}

/**
  * A disconnect causes the receiver to forget about this peer.
  */
final case class DisconnectMessage(proto: Protocol, timestamp: Long) extends ProtocolMessage

/**
  * The response to a lookup message. It holds the list of peers
  * closest to the queried key.
  */
final case class LookupResponseMessage(proto: Protocol, timestamp: Long) extends ProtocolMessage

final case class UpstreamMessage(proto: Protocol, timestamp: Long)  extends ProtocolMessage
final case class UpstreamResponse(proto: Protocol, timestamp: Long) extends ProtocolMessage

/**
  * Utility functions for working with protocol buffers.
  */
object ProtocolMessage {

  implicit def toProtocolBytes(x: String): ByteString =
    com.google.protobuf.ByteString.copyFromUtf8(x)
  implicit def toProtocolBytes(x: Array[Byte]): ByteString =
    com.google.protobuf.ByteString.copyFrom(x)
  implicit def toProtocolBytes(x: Seq[Byte]): ByteString =
    com.google.protobuf.ByteString.copyFrom(x.toArray)

  def header(src: ProtocolNode): Header =
    Header()
      .withSender(node(src))
      .withTimestamp(System.currentTimeMillis)

  def node(n: PeerNode): Node =
    Node()
      .withId(n.key)
      .withHost(n.endpoint.host)
      .withUdpPort(n.endpoint.udpPort)
      .withTcpPort(n.endpoint.tcpPort)

  def toPeerNode(n: Node): PeerNode =
    PeerNode(NodeIdentifier(n.id.toByteArray), Endpoint(n.host.toStringUtf8, n.tcpPort, n.udpPort))

  def ping(src: ProtocolNode): Protocol =
    Protocol()
      .withHeader(header(src))
      .withPing(Ping())

  def pong(src: ProtocolNode): Protocol =
    Protocol()
      .withHeader(header(src))
      .withPong(Pong())

  def lookup(src: ProtocolNode, id: Seq[Byte]): Protocol =
    Protocol()
      .withHeader(header(src))
      .withLookup(Lookup()
        .withId(id.toArray))

  def lookupResponse(src: ProtocolNode, nodes: Seq[PeerNode]): Protocol =
    Protocol()
      .withHeader(header(src))
      .withLookupResponse(LookupResponse()
        .withNodes(nodes.map(node)))

  def disconnect(src: ProtocolNode): Protocol =
    Protocol()
      .withHeader(header(src))
      .withDisconnect(Disconnect())

  def upstreamMessage(src: ProtocolNode, upstream: AnyProto): Protocol =
    Protocol()
      .withHeader(header(src))
      .withUpstream(upstream)

  def upstreamResponse(src: ProtocolNode, upstream: AnyProto): Protocol =
    Protocol()
      .withHeader(header(src))
      .withUpstream(upstream)

  def toProtocolMessage(proto: Protocol): Either[CommError, ProtocolMessage] = proto match {
    case msg: Protocol =>
      msg.message match {
        case Protocol.Message.Ping(_)   => Right(PingMessage(msg, System.currentTimeMillis))
        case Protocol.Message.Pong(_)   => Right(PongMessage(msg, System.currentTimeMillis))
        case Protocol.Message.Lookup(_) => Right(LookupMessage(msg, System.currentTimeMillis))
        case Protocol.Message.LookupResponse(_) =>
          Right(LookupResponseMessage(msg, System.currentTimeMillis))
        case Protocol.Message.Disconnect(_) =>
          Right(DisconnectMessage(msg, System.currentTimeMillis))
        case Protocol.Message.Upstream(_) => Right(UpstreamMessage(msg, System.currentTimeMillis))
        case _                            => Left(UnknownProtocolError("unable to unmarshal protocol buffer"))
      }
  }
  def parse(bytes: Seq[Byte]): Either[CommError, ProtocolMessage] =
    try {
      toProtocolMessage(Protocol.parseFrom(bytes.toArray))
    } catch {
      case NonFatal(ex: Exception) => Left(ProtocolException(ex))
    }
}
