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

  def packet(src: PeerNode, content: Array[Byte]): routing.Protocol =
    packet(src, ByteString.copyFrom(content))

  def packet(src: PeerNode, content: ByteString): routing.Protocol = {
    val p = Packet(content)
    ProtocolHelper.upstreamMessage(src, AnyProto.pack(p))
  }

  def toPacket(proto: routing.Protocol): CommErr[Packet] = proto.message match {
    case routing.Protocol.Message.Upstream(upstream) => Right(upstream.unpack(Packet))
    case a                                           => Left(UnknownProtocolError(s"Was expecting Packet, got $a"))
  }

}
