package coop.rchain.rholang.interpreter.accounting

import coop.rchain.models.Channel
import coop.rchain.models.Channel.ChannelInstance.Quote
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

  implicit val chargeableQuote: Chargeable[Quote] = new Chargeable[Quote] {
    override def cost(a: Quote): Int = Channel(a).serializedSize
  }

  implicit val chargeableChannel: Chargeable[Channel] = new Chargeable[Channel] {
    override def cost(a: Channel): Int = a.serializedSize
  }
}
