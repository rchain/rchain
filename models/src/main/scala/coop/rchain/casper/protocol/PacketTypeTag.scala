package coop.rchain.casper.protocol

import java.io.IOException

import com.google.protobuf.ByteString
import coop.rchain.comm.protocol.routing.Packet
import enumeratum._
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

import scala.util.{Failure, Try}

sealed abstract class PacketTypeTag extends EnumEntry

object PacketTypeTag extends Enum[PacketTypeTag] {
  case object BlockMessage             extends PacketTypeTag
  case object HasBlockRequest          extends PacketTypeTag
  case object HasBlock                 extends PacketTypeTag
  case object BlockRequest             extends PacketTypeTag
  case object ForkChoiceTipRequest     extends PacketTypeTag
  case object ApprovedBlock            extends PacketTypeTag
  case object ApprovedBlockRequest     extends PacketTypeTag
  case object BlockApproval            extends PacketTypeTag
  case object UnapprovedBlock          extends PacketTypeTag
  case object NoApprovedBlockAvailable extends PacketTypeTag

  override val values = findValues

  sealed abstract class ValueOf[Of <: PacketTypeTag] {
    def tag: String
  }
  object ValueOf {
    private def summon[A <: PacketTypeTag](a: A): ValueOf[A] = new ValueOf[A] {
      override val tag: String = a.entryName
    }
    def apply[A <: PacketTypeTag](implicit ev: ValueOf[A]) = ev

    implicit val valueOfBlockMessage: ValueOf[BlockMessage.type]       = summon(BlockMessage)
    implicit val valueOfHasBlockRequest: ValueOf[HasBlockRequest.type] = summon(HasBlockRequest)
    implicit val valueOfHasBlock: ValueOf[HasBlock.type]               = summon(HasBlock)
    implicit val valueOfBlockRequest: ValueOf[BlockRequest.type]       = summon(BlockRequest)
    implicit val valueOfApprovedBlock: ValueOf[ApprovedBlock.type]     = summon(ApprovedBlock)
    implicit val valueOfBlockApproval: ValueOf[BlockApproval.type]     = summon(BlockApproval)
    implicit val valueOfUnapprovedBlock: ValueOf[UnapprovedBlock.type] = summon(UnapprovedBlock)
    implicit val valueOfForkChoiceTipRequest: ValueOf[ForkChoiceTipRequest.type] = summon(
      ForkChoiceTipRequest
    )
    implicit val valueOfApprovedBlockRequest: ValueOf[ApprovedBlockRequest.type] = summon(
      ApprovedBlockRequest
    )
    implicit val valueOfApprovedBlockAvailable: ValueOf[NoApprovedBlockAvailable.type] = summon(
      NoApprovedBlockAvailable
    )
  }

}

import coop.rchain.casper.protocol.PacketTypeTag.ValueOf

trait FromPacket[Tag <: PacketTypeTag] {
  type To
  def witness: ValueOf[Tag]
  final def parseFrom(packet: Packet): Try[To] =
    if (packet.typeId == witness.tag) parse(packet.content.toByteArray)
    else
      Failure(new IOException(s"Cannot parse ${packet.typeId} packet - need ${witness.tag} packet"))
  protected def parse(content: Array[Byte]): Try[To]
}

object FromPacket {
  def protoImpl[Tag <: PacketTypeTag, A <: GeneratedMessage with Message[A]](
      implicit companion: GeneratedMessageCompanion[A],
      witness0: ValueOf[Tag]
  ): FromPacket[Tag] { type To = A } = new FromPacket[Tag] {
    override type To = A
    override val witness                               = witness0
    protected override def parse(content: Array[Byte]) = Try(companion.parseFrom(content))
  }
}

trait ToPacket[A] {
  type Tag <: PacketTypeTag
  def witness: ValueOf[Tag]
  final def mkPacket(model: A): Packet = Packet(witness.tag, content(model))
  protected def content(a: A): ByteString
}
object ToPacket {
  def apply[A](msg: A)(implicit ev: ToPacket[A]) = ev.mkPacket(msg)

  def protoMessageImpl[A <: GeneratedMessage, Tag0 <: PacketTypeTag](
      implicit witness0: ValueOf[Tag0]
  ): ToPacket[A] { type Tag = Tag0 } = new ToPacket[A] {
    override type Tag = Tag0
    override val witness                             = witness0
    protected override def content(a: A): ByteString = a.toByteString
  }
  implicit def protoSerde[Tag0 <: PacketTypeTag, A0 <: GeneratedMessage with Message[A0]](
      implicit de: FromPacket[Tag0] { type To = A0 }
  ): ToPacket[A0] { type Tag = Tag0 } = protoMessageImpl[A0, Tag0](de.witness)
}
