package coop.rchain.casper

import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.transport
import scala.util.Try

package object protocol {

  def toCasperMessageProto(packet: Packet): Option[CasperMessageProto] =
    packetToBlockRequest(packet) orElse
      packetToHasBlockRequest(packet) orElse
      packetToHasBlock(packet) orElse
      packetToForkChoiceTipRequest(packet) orElse
      packetToApprovedBlock(packet) orElse
      packetToApprovedBlockRequest(packet) orElse
      packetToBlockMessage(packet) orElse
      packetToBlockApproval(packet) orElse
      packetToUnapprovedBlock(packet) orElse
      packetToNoApprovedBlockAvailable(packet)

  private def packetToBlockMessage(msg: Packet): Option[BlockMessageProto] =
    convert[BlockMessageProto](msg, transport.BlockMessage, BlockMessageProto.parseFrom)
  private def packetToApprovedBlock(msg: Packet): Option[ApprovedBlockProto] =
    convert[ApprovedBlockProto](msg, transport.ApprovedBlock, ApprovedBlockProto.parseFrom)
  private def packetToApprovedBlockRequest(msg: Packet): Option[ApprovedBlockRequestProto] =
    convert[ApprovedBlockRequestProto](
      msg,
      transport.ApprovedBlockRequest,
      ApprovedBlockRequestProto.parseFrom
    )
  def packetToBlockRequest(msg: Packet): Option[BlockRequestProto] =
    convert[BlockRequestProto](msg, transport.BlockRequest, BlockRequestProto.parseFrom)
  def packetToHasBlockRequest(msg: Packet): Option[HasBlockRequestProto] =
    convert[HasBlockRequestProto](msg, transport.HasBlockRequest, HasBlockRequestProto.parseFrom)
  def packetToHasBlock(msg: Packet): Option[HasBlockProto] =
    convert[HasBlockProto](msg, transport.HasBlock, HasBlockProto.parseFrom)

  private def packetToForkChoiceTipRequest(msg: Packet): Option[ForkChoiceTipRequestProto] =
    convert[ForkChoiceTipRequestProto](
      msg,
      transport.ForkChoiceTipRequest,
      ForkChoiceTipRequestProto.parseFrom
    )
  private def packetToBlockApproval(msg: Packet): Option[BlockApprovalProto] =
    convert[BlockApprovalProto](msg, transport.BlockApproval, BlockApprovalProto.parseFrom)
  private def packetToUnapprovedBlock(msg: Packet): Option[UnapprovedBlockProto] =
    convert[UnapprovedBlockProto](msg, transport.UnapprovedBlock, UnapprovedBlockProto.parseFrom)
  private def packetToNoApprovedBlockAvailable(msg: Packet): Option[NoApprovedBlockAvailableProto] =
    convert[NoApprovedBlockAvailableProto](
      msg,
      transport.NoApprovedBlockAvailable,
      NoApprovedBlockAvailableProto.parseFrom
    )

  private def convert[A <: CasperMessageProto](
      msg: Packet,
      pt: transport.PacketType,
      parse: Array[Byte] => A
  ): Option[A] =
    if (msg.typeId == pt.id)
      Try(parse(msg.content.toByteArray)).toOption
    else None
}
