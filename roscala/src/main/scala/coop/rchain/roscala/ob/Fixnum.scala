package coop.rchain.roscala.ob

case class Fixnum(value: Int) extends Ob {
  override def toString = s"Fixnum($value)"

  def +(that: Fixnum) = Fixnum(this.value + that.value)

  def -(that: Fixnum) = Fixnum(this.value - that.value)

  def *(that: Fixnum) = Fixnum(this.value * that.value)

  def /(that: Fixnum) = Fixnum(this.value / that.value)

  def %(that: Fixnum) = Fixnum(this.value % that.value)

  def <(that: Fixnum) = RblBool(this.value < that.value)

  def >(that: Fixnum) = RblBool(this.value > that.value)

  def <=(that: Fixnum) = RblBool(this.value <= that.value)

  def >=(that: Fixnum) = RblBool(this.value >= that.value)

  def ==(that: Fixnum) = RblBool(this.value == that.value)

  def !=(that: Fixnum) = RblBool(this.value != that.value)

  def |(that: Fixnum) = Fixnum(this.value | that.value)

  def &(that: Fixnum) = Fixnum(this.value & that.value)

  def <<(that: Fixnum) = Fixnum(this.value << that.value)

  def >>(that: Fixnum) = Fixnum(this.value >> that.value)

  def >>>(that: Fixnum) = Fixnum(this.value >>> that.value)

  def ^(that: Fixnum) = Fixnum(this.value ^ that.value)
}

object Fixnum {
  val fixnumMeta = Meta(extensible = false)
  val fixnumSbo  = new Actor()

  def apply(value: Int): Fixnum = {
    val fixnum = new Fixnum(value)
    fixnum.parent = fixnumSbo
    fixnum.meta = fixnumMeta
    fixnum.meta.refCount.incrementAndGet()
    fixnum
  }
}
