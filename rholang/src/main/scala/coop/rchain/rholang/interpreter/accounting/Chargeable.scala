package coop.rchain.rholang.interpreter.accounting

import coop.rchain.models.ProtoM
import monix.eval.Coeval
import scalapb.GeneratedMessage

trait Chargeable[A] {
  def cost(a: A): Long
}

object Chargeable {
  def apply[T](implicit ev: Chargeable[T]): Chargeable[T] = ev

  implicit def fromProtobuf[T <: GeneratedMessage] =
    new Chargeable[T] {
      override def cost(a: T): Long = ProtoM.serializedSize[Coeval](a).value.toLong
    }
}
