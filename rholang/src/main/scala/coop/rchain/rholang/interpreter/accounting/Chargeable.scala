package coop.rchain.rholang.interpreter.accounting

import coop.rchain.models.{ProtoM, StacksafeMessage}

trait Chargeable[A] {
  def cost(a: A): Long
}

object Chargeable {
  def apply[T](implicit ev: Chargeable[T]): Chargeable[T] = ev

  implicit def fromProtobuf[T <: StacksafeMessage[_]] =
    new Chargeable[T] {
      override def cost(a: T): Long = ProtoM.serializedSize(a).value.toLong
    }
}
