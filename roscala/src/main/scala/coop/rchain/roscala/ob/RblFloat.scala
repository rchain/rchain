package coop.rchain.roscala.ob

case class RblFloat(value: Double) extends Ob {
  override def toString: String = s"Float($value)"

  def +(that: RblFloat) = RblFloat(this.value + that.value)

  def -(that: RblFloat) = RblFloat(this.value - that.value)

  def unary_-() = RblFloat(-this.value)

  def *(that: RblFloat) = RblFloat(this.value * that.value)

  def /(that: RblFloat) = RblFloat(this.value / that.value)

  def <(that: RblFloat) = RblBool(this.value < that.value)

  def <=(that: RblFloat) = RblBool(this.value <= that.value)

  def >(that: RblFloat) = RblBool(this.value > that.value)

  def >=(that: RblFloat) = RblBool(this.value >= that.value)

  def ==(that: RblFloat) = RblBool(this.value == that.value)

  def !=(that: RblFloat) = RblBool(this.value != that.value)
}

object RblFloat {
  val rblFloatMeta = Meta(extensible = false)
  val rblFloatSbo  = new Actor()

  def apply(value: Double): RblFloat = {
    val rblFloat = new RblFloat(value)
    rblFloat.parent = rblFloatSbo
    rblFloat.meta = rblFloatMeta
    rblFloat.meta.refCount.incrementAndGet()

    rblFloat
  }
}
