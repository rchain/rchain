package coop.rchain.rholang.interpreter.accounting

import coop.rchain.models.Par
import scalapb.{GeneratedMessage, Message}

trait Chargeable[A] {
  def cost(a: A): Int
}

object Chargeable {
  def apply[T](implicit ev: Chargeable[T]): Chargeable[T] = ev

  implicit def fromProtobuf[T <: GeneratedMessage with Message[T]] =
    new Chargeable[T] {
      override def cost(a: T): Int = a.serializedSize
    }

  implicit val chargeableQuote: Chargeable[Par] = new Chargeable[Par] {
    override def cost(a: Par): Int = a.serializedSize
  }
}
