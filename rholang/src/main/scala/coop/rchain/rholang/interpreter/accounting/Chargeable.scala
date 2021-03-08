package coop.rchain.rholang.interpreter.accounting

import coop.rchain.models.{StacksafeMessage}

/* TODO: Make Chargeable instances for requisite rspace type parameters. Then, create an instance of PureRSpace
         that uses the generic instances, _cost, and _error for a single, charging PureRSpace. */

trait Chargeable[A] {
  def cost(a: A): Long
}

object Chargeable {
  def apply[T](implicit ev: Chargeable[T]): Chargeable[T] = ev

  implicit def fromProtobuf[T <: StacksafeMessage[_]] =
    new Chargeable[T] {
//      override def cost(a: T): Long = ProtoM.serializedSize(a).value.toLong
      override def cost(a: T): Long = a.serializedSize.toLong
    }
}
