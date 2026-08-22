package coop.rchain.roscala.ob.expr

import coop.rchain.roscala.ob.Ob

class TupleExpr(elem: Seq[Ob], rest: Option[Ob] = None) extends Expr {
  def numberOfElements(): Int = elem.size
}
