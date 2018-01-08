package coop.rchain.rosette

import coop.rchain.rosette.Meta.StdMeta

case class RblFloat(value: Double, override val slot: Seq[Ob] = Seq(StdMeta()))
    extends RblAtom {
  override def toString: String = s"Float($value)"

  def +(that: RblFloat) = RblFloat(this.value + that.value)

  def -(that: RblFloat) = RblFloat(this.value - that.value)

  def *(that: RblFloat) = RblFloat(this.value * that.value)

  def /(that: RblFloat) = RblFloat(this.value / that.value)

  def <(that: RblFloat) = RblBool(this.value < that.value)

  def <=(that: RblFloat) = RblBool(this.value <= that.value)

  def >(that: RblFloat) = RblBool(this.value > that.value)

  def >=(that: RblFloat) = RblBool(this.value >= that.value)

  def ==(that: RblFloat) = RblBool(this.value == that.value)

  def !=(that: RblFloat) = RblBool(this.value != that.value)
}
