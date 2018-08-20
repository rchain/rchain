package coop.rchain.comm

import com.google.protobuf.ByteString
import com.google.protobuf.any.{Any => AnyProto}
import coop.rchain.comm.protocol.routing._

/**
  * Utility functions for working with protocol buffers.
  */
object ProtocolHelper {

  def sender(proto: Protocol): Option[PeerNode] =
    for {
      h <- proto.header
      s <- h.sender
    } yield
      PeerNode(NodeIdentifier(s.id.toByteArray),
               Endpoint(s.host.toStringUtf8, s.tcpPort, s.udpPort))

  implicit def toProtocolBytes(x: String): ByteString =
    com.google.protobuf.ByteString.copyFromUtf8(x)
  implicit def toProtocolBytes(x: Array[Byte]): ByteString =
    com.google.protobuf.ByteString.copyFrom(x)
  implicit def toProtocolBytes(x: Seq[Byte]): ByteString =
    com.google.protobuf.ByteString.copyFrom(x.toArray)

  def header(src: PeerNode, currentMillis: Long): Header =
    Header()
      .withSender(node(src))
      .withTimestamp(currentMillis)

  def node(n: PeerNode): Node =
    Node()
      .withId(n.key)
      .withHost(n.endpoint.host)
      .withUdpPort(n.endpoint.udpPort)
      .withTcpPort(n.endpoint.tcpPort)

  def toPeerNode(n: Node): PeerNode =
    PeerNode(NodeIdentifier(n.id.toByteArray), Endpoint(n.host.toStringUtf8, n.tcpPort, n.udpPort))

  def ping(src: PeerNode, currentMillis: Long): Protocol =
    Protocol()
      .withHeader(header(src, currentMillis))
      .withPing(Ping())

  def pong(src: PeerNode, currentMillis: Long): Protocol =
    Protocol()
      .withHeader(header(src, currentMillis))
      .withPong(Pong())

  def lookup(src: PeerNode, id: Seq[Byte], currentMillis: Long): Protocol =
    Protocol()
      .withHeader(header(src, currentMillis))
      .withLookup(Lookup()
        .withId(id.toArray))

  def lookupResponse(src: PeerNode, nodes: Seq[PeerNode], currentMillis: Long): Protocol =
    Protocol()
      .withHeader(header(src, currentMillis))
      .withLookupResponse(LookupResponse()
        .withNodes(nodes.map(node)))

  def upstreamMessage(src: PeerNode, upstream: AnyProto, currentMillis: Long): Protocol =
    Protocol()
      .withHeader(header(src, currentMillis))
      .withUpstream(upstream)

  def upstreamResponse(src: PeerNode, upstream: AnyProto, currentMillis: Long): Protocol =
    Protocol()
      .withHeader(header(src, currentMillis))
      .withUpstream(upstream)

}
