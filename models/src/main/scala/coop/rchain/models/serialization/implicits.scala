package coop.rchain.models.serialization

import cats.implicits._
import coop.rchain.models._
import coop.rchain.rspace.Serialize
import monix.eval.Coeval
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}
import scodec.bits.ByteVector

object implicits {
  def mkProtobufInstance[T <: GeneratedMessage with Message[T]: GeneratedMessageCompanion] =
    new Serialize[T] {

      override def encode(a: T): ByteVector =
        ByteVector.view(ProtoM.toByteArray[Coeval](a).value())

      override def decode(bytes: ByteVector): Either[Throwable, T] = {
        val comp: GeneratedMessageCompanion[T] = implicitly
        Either.catchNonFatal(comp.parseFrom(bytes.toArray))
      }
    }

  implicit val serializePar: Serialize[Par]         = mkProtobufInstance
  implicit val serializeVar: Serialize[Var]         = mkProtobufInstance
  implicit val serializeSend: Serialize[Send]       = mkProtobufInstance
  implicit val serializeReceive: Serialize[Receive] = mkProtobufInstance
  implicit val serializeNew: Serialize[New]         = mkProtobufInstance
  implicit val serializeExpr: Serialize[Expr]       = mkProtobufInstance
  implicit val serializeMatch: Serialize[Match]     = mkProtobufInstance
  implicit val serializeESet: Serialize[ESet]       = mkProtobufInstance
  implicit val serializeEMap: Serialize[EMap]       = mkProtobufInstance
}
