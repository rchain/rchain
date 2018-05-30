package coop.rchain.roscala.ob

import scala.collection.mutable

case class Fixnum(value: Int) extends Ob {
  def +(that: Fixnum) = Fixnum(this.value + that.value)
}

object Fixnum {
  val fixnumMeta = Meta(map = mutable.Map(), 0, extensible = false)
  val fixnumSbo  = new Actor()

  def apply(value: Int): Fixnum = {
    val fixnum = new Fixnum(value)
    fixnum.parent = fixnumSbo
    fixnum.meta = fixnumMeta
    fixnum.meta.refCount += 1
    fixnum
  }
}
