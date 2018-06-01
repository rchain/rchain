package coop.rchain.roscala.ob

import coop.rchain.roscala.ob.expr.TupleExpr

class Template(val keyTuple: Tuple, val pat: CompoundPattern, val keyMeta: Meta) extends Ob {
  def `match`(argvec: Tuple, nargs: Int): Option[Tuple] =
    pat.`match`(argvec, nargs)
}

abstract class CompoundPattern(expr: TupleExpr) {
  def `match`(argvec: Tuple, nargs: Int): Option[Tuple]
}

class IdVecPattern(expr: TupleExpr) extends CompoundPattern(expr) {
  override def `match`(argvec: Tuple, nargs: Int): Option[Tuple] = {
    val need = expr.numberOfElements()
    if (need == nargs) Some(argvec) else None
  }
}
