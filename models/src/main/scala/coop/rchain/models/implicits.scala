package coop.rchain.models

import cats.implicits._
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.rspace.{Serialize, Match => StorageMatch}

object implicits {
  def mkProtobufInstance[T <: GeneratedMessage with Message[T]](
      comp: GeneratedMessageCompanion[T]) = new Serialize[T] {

    override def encode(a: T): Array[Byte] =
      comp.toByteArray(a)

    override def decode(bytes: Array[Byte]): Either[Throwable, T] =
      Either.catchNonFatal(comp.parseFrom(bytes))
  }

  implicit val serializePar: Serialize[Par]         = mkProtobufInstance(Par)
  implicit val serializeChannel: Serialize[Channel] = mkProtobufInstance(Channel)
  implicit val serializeVar: Serialize[Var]         = mkProtobufInstance(Var)
  implicit val serializeSend: Serialize[Send]       = mkProtobufInstance(Send)
  implicit val serializeReceive: Serialize[Receive] = mkProtobufInstance(Receive)
  implicit val serializeNew: Serialize[New]         = mkProtobufInstance(New)
  implicit val serializeExpr: Serialize[Expr]       = mkProtobufInstance(Expr)
  implicit val serializeMatch: Serialize[Match]     = mkProtobufInstance(Match)
}
