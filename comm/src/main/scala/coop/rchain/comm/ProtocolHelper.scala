package coop.rchain.comm

import coop.rchain.comm.protocol.routing._
import scala.util.control.NonFatal

import com.google.protobuf.any.{Any => AnyProto}
import coop.rchain.catscontrib._
import Catscontrib._

import com.google.protobuf.ByteString

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

  def header(src: PeerNode): Header =
    Header()
      .withSender(node(src))

  def node(n: PeerNode): Node =
    Node()
      .withId(n.key)
      .withHost(n.endpoint.host)
      .withUdpPort(n.endpoint.udpPort)
      .withTcpPort(n.endpoint.tcpPort)

  def toPeerNode(n: Node): PeerNode =
    PeerNode(NodeIdentifier(n.id.toByteArray), Endpoint(n.host.toStringUtf8, n.tcpPort, n.udpPort))

  def ping(src: PeerNode): Protocol =
    Protocol()
      .withHeader(header(src))
      .withPing(Ping())

  def pong(src: PeerNode): Protocol =
    Protocol()
      .withHeader(header(src))
      .withPong(Pong())

  def lookup(src: PeerNode, id: Seq[Byte]): Protocol =
    Protocol()
      .withHeader(header(src))
      .withLookup(Lookup()
        .withId(id.toArray))

  def lookupResponse(src: PeerNode, nodes: Seq[PeerNode]): Protocol =
    Protocol()
      .withHeader(header(src))
      .withLookupResponse(LookupResponse()
        .withNodes(nodes.map(node)))

  def upstreamMessage(src: PeerNode, upstream: AnyProto): Protocol =
    Protocol()
      .withHeader(header(src))
      .withUpstream(upstream)

  def upstreamResponse(src: PeerNode, upstream: AnyProto): Protocol =
    Protocol()
      .withHeader(header(src))
      .withUpstream(upstream)

}
