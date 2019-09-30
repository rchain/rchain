package coop.rchain.casper

import coop.rchain.comm.protocol.routing.Packet

package object protocol extends CasperMessageProtocol {
  import PacketTypeTag._

  def toCasperMessageProto(packet: Packet): Option[CasperMessageProto] =
    PacketTypeTag
      .withNameOption(packet.typeId)
      .flatMap {
        case BlockMessage             => convert[BlockMessage.type](packet)
        case ApprovedBlock            => convert[ApprovedBlock.type](packet)
        case ApprovedBlockRequest     => convert[ApprovedBlockRequest.type](packet)
        case BlockRequest             => convert[BlockRequest.type](packet)
        case HasBlockRequest          => convert[HasBlockRequest.type](packet)
        case HasBlock                 => convert[HasBlock.type](packet)
        case ForkChoiceTipRequest     => convert[ForkChoiceTipRequest.type](packet)
        case BlockApproval            => convert[BlockApproval.type](packet)
        case UnapprovedBlock          => convert[UnapprovedBlock.type](packet)
        case NoApprovedBlockAvailable => convert[NoApprovedBlockAvailable.type](packet)
      }

  @inline def convert[Tag <: PacketTypeTag](
      msg: Packet
  )(implicit fromPacket: FromPacket[Tag]): Option[fromPacket.To] =
    fromPacket.parseFrom(msg).toOption

}
