package coop.rchain.rosette.expr

import coop.rchain.rosette.Ob

case class TupleExpr(meta: Ob, parent: Ob, elem: Seq[Ob], rest: Option[Ob]) extends Ob {
  def numberOfElements(): Int = elem.size
}

object TupleExpr {
  def apply(elem: Seq[Ob], rest: Option[Ob] = None): TupleExpr =
    TupleExpr(meta = null, parent = null, elem, rest)
}
