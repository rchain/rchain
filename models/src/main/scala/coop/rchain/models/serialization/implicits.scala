package coop.rchain.models.serialization

import com.google.protobuf.CodedInputStream
import coop.rchain.models._
import coop.rchain.shared.Serialize
import monix.eval.Coeval
import scalapb.GeneratedMessageCompanion
import scodec.bits.ByteVector

import scala.util.Try

object implicits {

  implicit def mkProtobufInstance[T <: StacksafeMessage[T]: GeneratedMessageCompanion] =
    new Serialize[T] {

      override def encode(a: T): ByteVector =
        ByteVector.view(a.toByteArray)

      override def decode(bytes: ByteVector): Either[Throwable, T] = {
        val companion = implicitly[GeneratedMessageCompanion[T]]
        val buffer    = CodedInputStream.newInstance(bytes.toArray)
//        companion.defaultInstance.mergeFromM[Coeval](buffer).runAttempt()
        Try(companion.merge(companion.defaultInstance, buffer)).toEither
      }
    }

}
