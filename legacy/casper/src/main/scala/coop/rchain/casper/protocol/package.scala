package coop.rchain.casper

import coop.rchain.comm.protocol.routing.Packet

package object protocol extends CasperMessageProtocol {
  import PacketTypeTag._

  def toCasperMessageProto(packet: Packet): PacketParseResult[CasperMessageProto] =
    PacketTypeTag
      .withNameOption(packet.typeId)
      .map {
        // Block messages
        case BlockHashMessage => convert[BlockHashMessage.type](packet)
        case BlockMessage     => convert[BlockMessage.type](packet)
        case BlockRequest     => convert[BlockRequest.type](packet)
        case HasBlockRequest  => convert[HasBlockRequest.type](packet)
        case HasBlock         => convert[HasBlock.type](packet)
        // Tips
        case ForkChoiceTipRequest => convert[ForkChoiceTipRequest.type](packet)
        // Finalized fringe
        case FinalizedFringe        => convert[FinalizedFringe.type](packet)
        case FinalizedFringeRequest => convert[FinalizedFringeRequest.type](packet)
        // Last finalized state messages
        case StoreItemsMessageRequest => convert[StoreItemsMessageRequest.type](packet)
        case StoreItemsMessage        => convert[StoreItemsMessage.type](packet)
      }
      .getOrElse(PacketParseResult.IllegalPacket(s"Unrecognized typeId: ${packet.typeId}"))

  @inline def convert[Tag <: PacketTypeTag](msg: Packet)(
      implicit fromPacket: FromPacket[Tag]
  ): PacketParseResult[fromPacket.To] = fromPacket.parseFrom(msg)

}
