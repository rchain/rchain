package coop.rchain.comm.rp

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.ToPacket
import coop.rchain.comm.CommError._
import coop.rchain.comm._
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.transport.Blob

import scala.collection.compat.immutable.ArraySeq

object ProtocolHelper {

  def toProtocolBytes(x: String): ByteString      = ByteString.copyFromUtf8(x)
  def toProtocolBytes(x: Array[Byte]): ByteString = ByteString.copyFrom(x)
  def toProtocolBytes(x: Seq[Byte]): ByteString   = ByteString.copyFrom(x.toArray)

  def header(src: PeerNode, networkId: String): Header =
    Header()
      .withSender(node(src))
      .withNetworkId(networkId)

  def node(n: PeerNode): Node =
    Node()
      .withId(ByteString.copyFrom(n.key.toArray))
      .withHost(ByteString.copyFromUtf8(n.endpoint.host))
      .withUdpPort(n.endpoint.udpPort)
      .withTcpPort(n.endpoint.tcpPort)

  def sender(proto: Protocol): PeerNode = toPeerNode(proto.header.sender)

  def toPeerNode(n: Node): PeerNode =
    PeerNode(
      NodeIdentifier(ArraySeq.unsafeWrapArray(n.id.toByteArray)),
      Endpoint(n.host.toStringUtf8, n.tcpPort, n.udpPort)
    )

  def protocol(src: PeerNode, networkId: String): Protocol =
    Protocol().withHeader(header(src, networkId))

  def protocolHandshake(src: PeerNode, networkId: String): Protocol =
    protocol(src, networkId).withProtocolHandshake(ProtocolHandshake())

  def toProtocolHandshake(proto: Protocol): CommErr[ProtocolHandshake] =
    proto.message.protocolHandshake.fold[CommErr[ProtocolHandshake]](
      Left(UnknownProtocolError(s"Was expecting ProtocolHandshake, got ${proto.message}"))
    )(Right(_))

  def protocolHandshakeResponse(src: PeerNode, networkId: String): Protocol =
    protocol(src, networkId).withProtocolHandshakeResponse(ProtocolHandshakeResponse())

  def heartbeat(src: PeerNode, networkId: String): Protocol =
    protocol(src, networkId).withHeartbeat(Heartbeat())

  def toHeartbeat(proto: Protocol): CommErr[Heartbeat] =
    proto.message.heartbeat.fold[CommErr[Heartbeat]](
      Left(UnknownProtocolError(s"Was expecting Heartbeat, got ${proto.message}"))
    )(Right(_))

  def packet(src: PeerNode, networkId: String, packet: Packet): Protocol =
    protocol(src, networkId).withPacket(packet)

  def packet[A](src: PeerNode, networkId: String, content: A)(
      implicit toPacket: ToPacket[A]
  ): Protocol = packet(src, networkId, toPacket.mkPacket(content))

  def toPacket(proto: Protocol): CommErr[Packet] =
    proto.message.packet.fold[CommErr[Packet]](
      Left(UnknownProtocolError(s"Was expecting Packet, got ${proto.message}"))
    )(Right(_))

  def disconnect(src: PeerNode, networkId: String): Protocol =
    protocol(src, networkId).withDisconnect(Disconnect())

  def toDisconnect(proto: Protocol): CommErr[Disconnect] =
    proto.message.disconnect.fold[CommErr[Disconnect]](
      Left(UnknownProtocolError(s"Was expecting Disconnect, got ${proto.message}"))
    )(Right(_))

  def blob(sender: PeerNode, typeId: String, content: Array[Byte]): Blob =
    Blob(
      sender,
      Packet()
        .withTypeId(typeId)
        .withContent(ProtocolHelper.toProtocolBytes(content))
    )

}
