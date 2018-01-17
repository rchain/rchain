package coop.rchain.rosette.expr

import coop.rchain.rosette.Ob

case class TupleExpr(elem: Seq[Ob], rest: Option[Ob] = None) extends Ob {
  def numberOfElements(): Int = elem.size
}
