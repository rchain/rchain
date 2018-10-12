package coop.rchain.comm.rp

import com.google.protobuf.ByteString
import com.google.protobuf.any.{Any => AnyProto}
import coop.rchain.comm.CommError._
import coop.rchain.comm._
import coop.rchain.comm.protocol.routing._
import com.google.protobuf.ByteString
import coop.rchain.comm.transport.PacketType
import com.google.protobuf.ByteString

object ProtocolHelper {

  def toProtocolBytes(x: String): ByteString      = ByteString.copyFromUtf8(x)
  def toProtocolBytes(x: Array[Byte]): ByteString = ByteString.copyFrom(x)
  def toProtocolBytes(x: Seq[Byte]): ByteString   = ByteString.copyFrom(x.toArray)

  def header(src: PeerNode): Header =
    Header()
      .withSender(node(src))

  def node(n: PeerNode): Node =
    Node()
      .withId(ByteString.copyFrom(n.key.toArray))
      .withHost(ByteString.copyFromUtf8(n.endpoint.host))
      .withUdpPort(n.endpoint.udpPort)
      .withTcpPort(n.endpoint.tcpPort)

  def sender(proto: Protocol): Option[PeerNode] =
    for {
      h <- proto.header
      s <- h.sender
    } yield toPeerNode(s)

  def toPeerNode(n: Node): PeerNode =
    PeerNode(NodeIdentifier(n.id.toByteArray), Endpoint(n.host.toStringUtf8, n.tcpPort, n.udpPort))

  def protocol(src: PeerNode): Protocol =
    Protocol().withHeader(header(src))

  def protocolHandshake(src: PeerNode): Protocol =
    protocol(src).withProtocolHandshake(ProtocolHandshake())

  def toProtocolHandshake(proto: Protocol): CommErr[ProtocolHandshake] =
    proto.message.protocolHandshake.fold[CommErr[ProtocolHandshake]](
      Left(UnknownProtocolError(s"Was expecting ProtocolHandshake, got ${proto.message}"))
    )(Right(_))

  def protocolHandshakeResponse(src: PeerNode): Protocol =
    protocol(src).withProtocolHandshakeResponse(ProtocolHandshakeResponse())

  def heartbeat(src: PeerNode): Protocol =
    protocol(src).withHeartbeat(Heartbeat())

  def toHeartbeat(proto: Protocol): CommErr[Heartbeat] =
    proto.message.heartbeat.fold[CommErr[Heartbeat]](
      Left(UnknownProtocolError(s"Was expecting Heartbeat, got ${proto.message}"))
    )(Right(_))

  def heartbeatResponse(src: PeerNode): Protocol =
    protocol(src).withHeartbeatResponse(HeartbeatResponse())

  def packet(src: PeerNode, pType: PacketType, content: Array[Byte]): Protocol =
    packet(src, pType, ByteString.copyFrom(content))

  def packet(src: PeerNode, pType: PacketType, content: ByteString): Protocol =
    protocol(src).withPacket(Packet(pType.id, content))

  def toPacket(proto: Protocol): CommErr[Packet] =
    proto.message.packet.fold[CommErr[Packet]](
      Left(UnknownProtocolError(s"Was expecting Packet, got ${proto.message}"))
    )(Right(_))

  def disconnect(src: PeerNode): Protocol =
    protocol(src).withDisconnect(Disconnect())

  def toDisconnect(proto: Protocol): CommErr[Disconnect] =
    proto.message.disconnect.fold[CommErr[Disconnect]](
      Left(UnknownProtocolError(s"Was expecting Disconnect, got ${proto.message}"))
    )(Right(_))

}
