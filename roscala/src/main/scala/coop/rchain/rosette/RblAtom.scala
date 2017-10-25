package coop.rchain.rosette

trait RblAtom extends Ob

case class Fixnum(value: Int,
                  override val parent: Ob = null,
                  override val meta: Ob = null,
                  override val slot: Seq[Ob] = null,
                  override val obTag: Ob.ObTag = Ob.OTfixnum)
    extends RblAtom
