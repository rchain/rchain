package coop.rchain.comm

import coop.rchain.kademlia
import coop.rchain.comm.protocol.routing._
import scala.util.{Failure, Success, Try}
import scala.concurrent.duration.{Duration, MILLISECONDS}

/**
  * Implements broadcasting and round-trip (request-response) messaging
  * for higher level protocols.
  */
trait ProtocolHandler {

  /**
    * The node that anchors this handler; `local` becomes the source
    * for outgoing communications.
    */
  def local: ProtocolNode

  /**
    * Send a message to a single, remote node, and wait up to the
    * specified duration for a response.
    */
  def roundTrip(msg: ProtocolMessage,
                remote: ProtocolNode,
                timeout: Duration = Duration(500, MILLISECONDS)): Try[ProtocolMessage]

  /**
    * Asynchronously broadcast a message to all known peers.
    */
  def broadcast(msg: ProtocolMessage): Seq[Try[Unit]]
}

/**
  * A `PeerNode` that knows how to send and receive messages. The
  * `ping` method for Kademlia is here.
  */
class ProtocolNode(id: NodeIdentifier, endpoint: Endpoint, handler: ProtocolHandler)
    extends PeerNode(id, endpoint)
    with kademlia.Peer {

  def this(peer: PeerNode, handler: ProtocolHandler) =
    this(peer.id, peer.endpoint, handler)

  private var _seq = 0L
  def seq: Long = _seq synchronized {
    _seq += 1
    _seq
  }

  override def ping: Try[Duration] =
    ProtocolMessage.ping(handler.local) match {
      case Some(ping) =>
        handler.roundTrip(PingMessage(ping, System.currentTimeMillis), this) match {
          case Success(pong) =>
            ping.header match {
              case Some(incoming) =>
                Success(Duration(pong.timestamp - incoming.timestamp, MILLISECONDS))
              case _ => Failure(new Exception("ping failed"))
            }
          case Failure(ex) => Failure(ex)
        }
      case None => Failure(new Exception("ping failed"))
    }
}

/**
  * `ProtocolMessage` insulates protocol handlers from protocol buffer
  * clutter.
  */
trait ProtocolMessage {
  val proto: Protocol
  val timestamp: Long
}

case class PingMessage(proto: Protocol, timestamp: Long) extends ProtocolMessage
case class PongMessage(proto: Protocol, timestamp: Long) extends ProtocolMessage

/**
  * Utility functions for working with protocol buffers.
  */
object ProtocolMessage {

  implicit def toProtocolBytes(x: String) =
    com.google.protobuf.ByteString.copyFromUtf8(x)
  implicit def toProtocolBytes(x: Array[Byte]) =
    com.google.protobuf.ByteString.copyFrom(x)
  implicit def toProtocolBytes(x: Seq[Byte]) =
    com.google.protobuf.ByteString.copyFrom(x.toArray)

  def toPeer(header: Header): Option[PeerNode] =
    for {
      node <- header.sender
    } yield
      PeerNode(NodeIdentifier(node.id.toByteArray),
               Endpoint(node.host.toStringUtf8, node.tcpPort, node.udpPort))

  def sender(msg: ProtocolMessage): Option[PeerNode] =
    for {
      header <- msg.proto.header
      sender <- toPeer(header)
    } yield sender

  def header(src: ProtocolNode) =
    Header()
      .withSender(node(src))
      .withTimestamp(System.currentTimeMillis)
      .withSeq(src.seq)

  def header(msg: ProtocolMessage): Option[Header] = msg.proto.header

  def returnHeader(msg: ProtocolMessage): Option[ReturnHeader] =
    for {
      proto <- msg.proto.message.pong
      ret <- proto.returnHeader
    } yield ret

  def node(n: ProtocolNode) =
    Node()
      .withId(n.key)
      .withHost(n.endpoint.host)
      .withUdpPort(n.endpoint.udpPort)
      .withTcpPort(n.endpoint.tcpPort)

  def returnHeader(h: Header) =
    ReturnHeader()
      .withTimestamp(h.timestamp)
      .withSeq(h.seq)

  def ping(src: ProtocolNode): Option[Protocol] =
    Some(Protocol().withHeader(header(src)).withPing(Ping()))

  def pong(src: ProtocolNode, h: Header): Protocol =
    Protocol()
      .withHeader(header(src))
      .withPong(Pong()
        .withReturnHeader(returnHeader(h)))

  def pong(src: ProtocolNode, ping: PingMessage): Option[Protocol] =
    for {
      h <- ping.proto.header
    } yield pong(src, h)

  def toBytes(proto: Protocol): Array[Byte] = {
    val buf = new java.io.ByteArrayOutputStream
    proto.writeTo(buf)
    buf.toByteArray
  }

  def toBytes(msg: ProtocolMessage): Array[Byte] = toBytes(msg.proto)

  def parse(bytes: Seq[Byte]): Option[ProtocolMessage] =
    Protocol.parseFrom(bytes.toArray) match {
      case msg: Protocol =>
        msg.message match {
          case Protocol.Message.Ping(p) => Some(PingMessage(msg, System.currentTimeMillis))
          case Protocol.Message.Pong(p) => Some(PongMessage(msg, System.currentTimeMillis))
          case _                        => None
        }
    }
}
