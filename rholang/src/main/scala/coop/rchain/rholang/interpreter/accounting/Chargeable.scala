package coop.rchain.rholang.interpreter.accounting

import scalapb.GeneratedMessage

trait Chargeable[A] {
  def cost(a: A): Int
}

object Chargeable {
  def apply[T](implicit ev: Chargeable[T]): Chargeable[T] = ev

  implicit def fromProtobuf[T <: GeneratedMessage] =
    new Chargeable[T] {
      override def cost(a: T): Int = a.serializedSize
    }
}
