package coop.rchain.comm.transport

import coop.rchain.comm._, CommError._
import com.google.protobuf.ByteString
import com.google.protobuf.any.{Any => AnyProto}
import coop.rchain.comm.protocol.rchain._
import coop.rchain.comm.protocol.routing
import coop.rchain.p2p.effects._
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._

object CommMessages {

  def protocolHandshake(src: PeerNode): routing.Protocol = {
    val ph = ProtocolHandshake()
    ProtocolHelper.upstreamMessage(src, AnyProto.pack(ph))
  }

  def toProtocolHandshake(proto: routing.Protocol): CommErr[ProtocolHandshake] =
    proto.message match {
      case routing.Protocol.Message.Upstream(upstream) => Right(upstream.unpack(ProtocolHandshake))
      case a                                           => Left(UnknownProtocolError(s"Was expecting Packet, got $a"))
    }

  def protocolHandshakeResponse(src: PeerNode): routing.Protocol = {
    val phr = ProtocolHandshakeResponse()
    ProtocolHelper.upstreamMessage(src, AnyProto.pack(phr))
  }

  def packet(src: PeerNode, pType: PacketType, content: Array[Byte]): routing.Protocol =
    packet(src, pType, ByteString.copyFrom(content))

  def heartbeat(src: PeerNode): routing.Protocol = {
    val hb = Heartbeat()
    ProtocolHelper.upstreamMessage(src, AnyProto.pack(hb))
  }

  def toHeartbeat(proto: routing.Protocol): CommErr[Heartbeat] =
    proto.message match {
      case routing.Protocol.Message.Upstream(upstream) => Right(upstream.unpack(Heartbeat))
      case a                                           => Left(UnknownProtocolError(s"Was expecting Heartbeat, got $a"))
    }

  def heartbeatResponse(src: PeerNode): routing.Protocol = {
    val hbr = HeartbeatResponse()
    ProtocolHelper.upstreamMessage(src, AnyProto.pack(hbr))
  }

  def packet(src: PeerNode, pType: PacketType, content: ByteString): routing.Protocol = {
    val p = Packet(pType.id, content)
    ProtocolHelper.upstreamMessage(src, AnyProto.pack(p))
  }

  def toPacket(proto: routing.Protocol): CommErr[Packet] = proto.message match {
    case routing.Protocol.Message.Upstream(upstream) => Right(upstream.unpack(Packet))
    case a                                           => Left(UnknownProtocolError(s"Was expecting Packet, got $a"))
  }

  def disconnect(src: PeerNode): routing.Protocol = {
    val d = Disconnect()
    ProtocolHelper.upstreamMessage(src, AnyProto.pack(d))
  }

  def toDisconnect(proto: routing.Protocol): CommErr[Disconnect] = proto.message match {
    case routing.Protocol.Message.Upstream(upstream) => Right(upstream.unpack(Disconnect))
    case a                                           => Left(UnknownProtocolError(s"Was expecting Disconnect, got $a"))
  }

}
