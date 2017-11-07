package coop.rchain.rosette

import scala.collection.mutable

trait RblAtom extends Ob

case class Fixnum(value: Int,
                  override val _slot: mutable.Seq[Ob] = null,
                  override val obTag: Ob.ObTag = Ob.OTfixnum)
    extends RblAtom
