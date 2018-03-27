package coop.rchain.rspace

import cats.syntax.either._
import com.trueaccord.scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

/**
  * Type class for serializing and deserializing values
  *
  * @tparam A The type to serialize and deserialize.
  */
trait Serialize[A] {

  def encode(a: A): Array[Byte]

  def decode(bytes: Array[Byte]): Either[Throwable, A]
}

object Serialize {

  def mkProtobufInstance[T <: GeneratedMessage with Message[T]](
      comp: GeneratedMessageCompanion[T]) = new Serialize[T] {

    override def encode(a: T): Array[Byte] =
      comp.toByteArray(a)

    override def decode(bytes: Array[Byte]): Either[Throwable, T] =
      Either.catchNonFatal(comp.parseFrom(bytes))
  }
}
