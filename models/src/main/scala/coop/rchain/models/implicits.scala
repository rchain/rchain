package coop.rchain.models

import cats.syntax.either._
import com.trueaccord.scalapb.{GeneratedMessage, GeneratedMessageCompanion}

object implicits {
  case class rhoInstanceWrapper[T <: GeneratedMessage with com.trueaccord.scalapb.Message[T]](
      companion: GeneratedMessageCompanion[T])
      extends Serialize[T] {
    override def encode(a: T): Array[Byte] = companion.toByteArray(a)

    override def decode(bytes: Array[Byte]): Either[Throwable, T] =
      Either.catchNonFatal(companion.parseFrom(bytes))
  }

  implicit object parInstance     extends rhoInstanceWrapper(Par)
  implicit object channelInstance extends rhoInstanceWrapper(Channel)
  implicit object varInstance     extends rhoInstanceWrapper(Var)
  implicit object sendInstance    extends rhoInstanceWrapper(Send)
  implicit object receiveInstance extends rhoInstanceWrapper(Receive)
  implicit object evalInstance    extends rhoInstanceWrapper(Eval)
  implicit object newInstance     extends rhoInstanceWrapper(New)
  implicit object exprInstance    extends rhoInstanceWrapper(Expr)
  implicit object matchInstance   extends rhoInstanceWrapper(Match)
}
