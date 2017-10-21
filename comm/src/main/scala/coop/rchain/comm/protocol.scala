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

  override def ping: Try[Duration] = {
    val req = PingMessage(ProtocolMessage.ping(handler.local), System.currentTimeMillis)
    handler.roundTrip(req, this) match {
      case Success(resp) =>
        req.header match {
          case Some(incoming) =>
            Success(Duration(resp.timestamp - incoming.timestamp, MILLISECONDS))
          case _ => Failure(new Exception("ping failed"))
        }
      case Failure(ex) => Failure(ex)
    }
  }
}

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

  def toByteSeq: Seq[Byte] = {
    val buf = new java.io.ByteArrayOutputStream
    proto.writeTo(buf)
    buf.toByteArray
  }
}

trait ProtocolResponse extends ProtocolMessage {
  def returnHeader: Option[ReturnHeader] = proto.returnHeader
}

case class PingMessage(proto: Protocol, timestamp: Long) extends ProtocolMessage {
  def response(src: ProtocolNode): Option[ProtocolMessage] =
    for {
      h <- header
    } yield PongMessage(ProtocolMessage.pong(src, h), System.currentTimeMillis)
}

case class PongMessage(proto: Protocol, timestamp: Long) extends ProtocolResponse

case class LookupMessage(proto: Protocol, timestamp: Long) extends ProtocolMessage {
  def lookupId: Option[Seq[Byte]] =
    for {
      lookup <- proto.message.lookup
    } yield lookup.id.toByteArray

  def response(src: ProtocolNode, nodes: Seq[PeerNode]): Option[ProtocolMessage] =
    for {
      h <- header
    } yield
      LookupResponseMessage(ProtocolMessage.lookupResponse(src, h, nodes),
                            System.currentTimeMillis)

}

case class LookupResponseMessage(proto: Protocol, timestamp: Long) extends ProtocolResponse

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

  def header(src: ProtocolNode): Header =
    Header()
      .withSender(node(src))
      .withTimestamp(System.currentTimeMillis)
      .withSeq(src.seq)

  def node(n: PeerNode): Node =
    Node()
      .withId(n.key)
      .withHost(n.endpoint.host)
      .withUdpPort(n.endpoint.udpPort)
      .withTcpPort(n.endpoint.tcpPort)

  def returnHeader(h: Header): ReturnHeader =
    ReturnHeader()
      .withTimestamp(h.timestamp)
      .withSeq(h.seq)

  def ping(src: ProtocolNode): Protocol =
    Protocol()
      .withHeader(header(src))
      .withPing(Ping())

  def pong(src: ProtocolNode, h: Header): Protocol =
    Protocol()
      .withHeader(header(src))
      .withReturnHeader(returnHeader(h))
      .withPong(Pong())

  def lookup(src: ProtocolNode, id: Seq[Byte]): Protocol =
    Protocol()
      .withHeader(header(src))
      .withLookup(Lookup()
        .withId(id.toArray))

  def lookupResponse(src: ProtocolNode, h: Header, nodes: Seq[PeerNode]): Protocol =
    Protocol()
      .withHeader(header(src))
      .withReturnHeader(returnHeader(h))
      .withLookupResponse(LookupResponse()
        .withNodes(nodes.map(node(_))))

  def parse(bytes: Seq[Byte]): Option[ProtocolMessage] =
    Protocol.parseFrom(bytes.toArray) match {
      case msg: Protocol =>
        msg.message match {
          case Protocol.Message.Ping(p)   => Some(PingMessage(msg, System.currentTimeMillis))
          case Protocol.Message.Pong(p)   => Some(PongMessage(msg, System.currentTimeMillis))
          case Protocol.Message.Lookup(_) => Some(LookupMessage(msg, System.currentTimeMillis))
          case Protocol.Message.LookupResponse(_) =>
            Some(LookupResponseMessage(msg, System.currentTimeMillis))
          case _ => None
        }
    }
}
