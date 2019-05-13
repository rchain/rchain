package coop.rchain.casper

import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.transport
import scala.util.Try

package object protocol {

  def toCasperMessage(packet: Packet): Option[CasperMessage] =
    packetToBlockRequest(packet) orElse
      packetToForkChoiceTipRequest(packet) orElse
      packetToApprovedBlock(packet) orElse
      packetToApprovedBlockRequest(packet) orElse
      packetToBlockMessage(packet) orElse
      packetToBlockApproval(packet) orElse
      packetToUnapprovedBlock(packet) orElse
      packetToNoApprovedBlockAvailable(packet)

  private def packetToBlockMessage(msg: Packet): Option[BlockMessage] =
    convert[BlockMessage](msg, transport.BlockMessage, BlockMessage.parseFrom)
  private def packetToApprovedBlock(msg: Packet): Option[ApprovedBlock] =
    convert[ApprovedBlock](msg, transport.ApprovedBlock, ApprovedBlock.parseFrom)
  private def packetToApprovedBlockRequest(msg: Packet): Option[ApprovedBlockRequest] =
    convert[ApprovedBlockRequest](
      msg,
      transport.ApprovedBlockRequest,
      ApprovedBlockRequest.parseFrom
    )
  private def packetToBlockRequest(msg: Packet): Option[BlockRequest] =
    convert[BlockRequest](msg, transport.BlockRequest, BlockRequest.parseFrom)
  private def packetToForkChoiceTipRequest(msg: Packet): Option[ForkChoiceTipRequest] =
    convert[ForkChoiceTipRequest](
      msg,
      transport.ForkChoiceTipRequest,
      ForkChoiceTipRequest.parseFrom
    )
  private def packetToBlockApproval(msg: Packet): Option[BlockApproval] =
    convert[BlockApproval](msg, transport.BlockApproval, BlockApproval.parseFrom)
  private def packetToUnapprovedBlock(msg: Packet): Option[UnapprovedBlock] =
    convert[UnapprovedBlock](msg, transport.UnapprovedBlock, UnapprovedBlock.parseFrom)
  private def packetToNoApprovedBlockAvailable(msg: Packet): Option[NoApprovedBlockAvailable] =
    convert[NoApprovedBlockAvailable](
      msg,
      transport.NoApprovedBlockAvailable,
      NoApprovedBlockAvailable.parseFrom
    )

  private def convert[A](
      msg: Packet,
      pt: transport.PacketType,
      parse: Array[Byte] => A
  ): Option[A] =
    if (msg.typeId == pt.id)
      Try(parse(msg.content.toByteArray)).toOption
    else None
}
