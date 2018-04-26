package coop.rchain.rosette

import coop.rchain.rosette.Meta.StdMeta

case class Fixnum(value: Int, meta: Ob = StdMeta(), parent: Ob = null) extends RblAtom {
  override def toString = s"Fixnum($value)"

  override def equals(obj: scala.Any) =
    obj match {
      case Fixnum(value, _, _) => value == this.value
      case _                   => false
    }

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
