package coop.rchain.roscala.ob

import scala.collection.mutable

case class RblFloat(value: Double) extends Ob {
  def +(that: RblFloat) = RblFloat(this.value + that.value)
}

object RblFloat {
  val rblFloatMeta = Meta(map = mutable.Map(), 0, extensible = false)
  val rblFloatSbo  = new Actor()

  def apply(value: Double): RblFloat = {
    val rblFloat = new RblFloat(value)
    rblFloat.parent = rblFloatSbo
    rblFloat.meta = rblFloatMeta
    rblFloat.meta.refCount += 1

    rblFloat
  }
}
