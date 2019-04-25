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
    if (msg.typeId == transport.BlockMessage.id)
      Try(BlockMessage.parseFrom(msg.content.toByteArray)).toOption
    else None

  private def packetToApprovedBlock(msg: Packet): Option[ApprovedBlock] =
    if (msg.typeId == transport.ApprovedBlock.id)
      Try(ApprovedBlock.parseFrom(msg.content.toByteArray)).toOption
    else None

  private def packetToApprovedBlockRequest(msg: Packet): Option[ApprovedBlockRequest] =
    if (msg.typeId == transport.ApprovedBlockRequest.id)
      Try(ApprovedBlockRequest.parseFrom(msg.content.toByteArray)).toOption
    else None

  private def packetToBlockRequest(msg: Packet): Option[BlockRequest] =
    if (msg.typeId == transport.BlockRequest.id)
      Try(BlockRequest.parseFrom(msg.content.toByteArray)).toOption
    else None

  private def packetToForkChoiceTipRequest(msg: Packet): Option[ForkChoiceTipRequest] =
    if (msg.typeId == transport.ForkChoiceTipRequest.id)
      Try(ForkChoiceTipRequest.parseFrom(msg.content.toByteArray)).toOption
    else None

  private def packetToBlockApproval(msg: Packet): Option[BlockApproval] =
    if (msg.typeId == transport.BlockApproval.id)
      Try(BlockApproval.parseFrom(msg.content.toByteArray)).toOption
    else None

  private def packetToUnapprovedBlock(msg: Packet): Option[UnapprovedBlock] =
    if (msg.typeId == transport.UnapprovedBlock.id)
      Try(UnapprovedBlock.parseFrom(msg.content.toByteArray)).toOption
    else None

  private def packetToNoApprovedBlockAvailable(msg: Packet): Option[NoApprovedBlockAvailable] =
    if (msg.typeId == transport.NoApprovedBlockAvailable.id)
      Try(NoApprovedBlockAvailable.parseFrom(msg.content.toByteArray)).toOption
    else None
}
