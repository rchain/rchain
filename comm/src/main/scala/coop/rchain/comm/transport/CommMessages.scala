package coop.rchain.comm.transport

import com.google.protobuf.ByteString
import com.google.protobuf.any.{Any => AnyProto}
import coop.rchain.comm.CommError._
import coop.rchain.comm._
import coop.rchain.comm.protocol.rchain._
import coop.rchain.comm.protocol.routing

object CommMessages {

  def protocolHandshake(src: PeerNode, currentMillis: Long): routing.Protocol = {
    val ph = ProtocolHandshake()
    ProtocolHelper.upstreamMessage(src, AnyProto.pack(ph), currentMillis)
  }

  def toProtocolHandshake(proto: routing.Protocol): CommErr[ProtocolHandshake] =
    proto.message match {
      case routing.Protocol.Message.Upstream(upstream) => Right(upstream.unpack(ProtocolHandshake))
      case a                                           => Left(UnknownProtocolError(s"Was expecting Packet, got $a"))
    }

  def protocolHandshakeResponse(src: PeerNode, currentMillis: Long): routing.Protocol = {
    val phr = ProtocolHandshakeResponse()
    ProtocolHelper.upstreamMessage(src, AnyProto.pack(phr), currentMillis)
  }

  def packet(src: PeerNode,
             pType: PacketType,
             content: Array[Byte],
             currentMillis: Long): routing.Protocol =
    packet(src, pType, ByteString.copyFrom(content), currentMillis)

  def heartbeat(src: PeerNode, currentMillis: Long): routing.Protocol = {
    val hb = Heartbeat()
    ProtocolHelper.upstreamMessage(src, AnyProto.pack(hb), currentMillis)
  }

  def toHeartbeat(proto: routing.Protocol): CommErr[Heartbeat] =
    proto.message match {
      case routing.Protocol.Message.Upstream(upstream) => Right(upstream.unpack(Heartbeat))
      case a                                           => Left(UnknownProtocolError(s"Was expecting Heartbeat, got $a"))
    }

  def heartbeatResponse(src: PeerNode, currentMillis: Long): routing.Protocol = {
    val hbr = HeartbeatResponse()
    ProtocolHelper.upstreamMessage(src, AnyProto.pack(hbr), currentMillis)
  }

  def packet(src: PeerNode,
             pType: PacketType,
             content: ByteString,
             currentMillis: Long): routing.Protocol = {
    val p = Packet(pType.id, content)
    ProtocolHelper.upstreamMessage(src, AnyProto.pack(p), currentMillis)
  }

  def toPacket(proto: routing.Protocol): CommErr[Packet] = proto.message match {
    case routing.Protocol.Message.Upstream(upstream) => Right(upstream.unpack(Packet))
    case a                                           => Left(UnknownProtocolError(s"Was expecting Packet, got $a"))
  }

}
