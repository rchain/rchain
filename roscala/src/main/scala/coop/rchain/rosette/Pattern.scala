package coop.rchain.rosette

import coop.rchain.rosette.expr.TupleExpr

class Pattern

abstract class CompoundPattern(expr: TupleExpr) extends Pattern {
  def `match`(argvec: Tuple, nargs: Int): Option[Tuple]
}

case class IdVecPattern(expr: TupleExpr) extends CompoundPattern(expr) {
  override def `match`(argvec: Tuple, nargs: Int): Option[Tuple] = {
    val need = expr.numberOfElements()
    if (need == nargs) Some(argvec) else None
  }
}
