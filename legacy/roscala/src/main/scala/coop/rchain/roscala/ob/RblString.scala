package coop.rchain.roscala.ob

case class RblString(value: String) extends Ob {}

object RblString {
  val rblStringMeta = Meta(extensible = false)
  val rblStringSbo  = new Actor()

  def apply(value: String): RblString = {
    val rblString = new RblString(value)
    rblString.parent = rblStringSbo
    rblString.meta = rblStringMeta
    rblString.meta.refCount.incrementAndGet()

    rblString
  }
}
