package coop.rchain.rosette.expr

import coop.rchain.rosette.{Ob, Slot}

case class TupleExpr(elem: Seq[Ob],
                     rest: Option[Ob] = None,
                     override val slot: Slot = Slot.Placeholder)
    extends Ob {
  def numberOfElements(): Int = elem.size
}
