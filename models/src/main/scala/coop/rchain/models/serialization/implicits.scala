package coop.rchain.models.serialization

import com.google.protobuf.CodedInputStream
import coop.rchain.models._
import coop.rchain.rspace.Serialize
import monix.eval.Coeval
import scalapb.GeneratedMessageCompanion
import scodec.bits.ByteVector

object implicits {
  def mkProtobufInstance[T <: StacksafeMessage[T]: GeneratedMessageCompanion] =
    new Serialize[T] {

      override def encode(a: T): ByteVector =
        ByteVector.view(ProtoM.toByteArray(a).value())

      override def decode(bytes: ByteVector): Either[Throwable, T] = {
        val companion = implicitly[GeneratedMessageCompanion[T]]
        val buffer    = CodedInputStream.newInstance(bytes.toArray)
        companion.defaultInstance.mergeFromM[Coeval](buffer).runAttempt()
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
