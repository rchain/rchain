package coop.rchain.storage.models

import java.nio.charset.StandardCharsets

import cats.syntax.either._
import com.trueaccord.scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import coop.rchain.storage.{Serialize, SerializeError}

trait SerializeInstances {

  implicit object stringInstance extends Serialize[String] {

    def encode(s: String): Array[Byte] =
      s.getBytes(StandardCharsets.UTF_8)

    def decode(bytes: Array[Byte]): Either[SerializeError, String] =
      Either
        .catchNonFatal(new String(bytes, StandardCharsets.UTF_8))
        .leftMap(SerializeError.apply)
  }

  case class rhoInstanceWrapper[T <: GeneratedMessage with com.trueaccord.scalapb.Message[T]](
      companion: GeneratedMessageCompanion[T])
      extends Serialize[T] {
    override def encode(a: T): Array[Byte] = companion.toByteArray(a)

    override def decode(bytes: Array[Byte]): Either[SerializeError, T] =
      Either
        .catchNonFatal(companion.parseFrom(bytes))
        .leftMap(SerializeError.apply)
  }

  implicit object parInstance      extends rhoInstanceWrapper(Par)
  implicit object boundVarInstance extends rhoInstanceWrapper(BoundVar)
  implicit object varInstance      extends rhoInstanceWrapper(Var)
}
