package coop.rchain.rosette

import coop.rchain.rosette.Meta.StdMeta

import scala.collection.mutable

trait RblAtom extends Ob

case class Fixnum(value: Int,
                  override val _slot: mutable.Seq[Ob] = mutable.Seq(StdMeta()),
                  override val obTag: Ob.ObTag = Ob.OTfixnum)
    extends RblAtom {
  def +(that: Fixnum) = Fixnum(this.value + that.value)

  def -(that: Fixnum) = Fixnum(this.value - that.value)

  def *(that: Fixnum) = Fixnum(this.value * that.value)

  def /(that: Fixnum) = Fixnum(this.value / that.value)
}
