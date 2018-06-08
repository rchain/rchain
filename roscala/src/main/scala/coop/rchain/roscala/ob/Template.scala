package coop.rchain.roscala.ob

import coop.rchain.roscala.ob.expr.TupleExpr

class Template(val keyTuple: Tuple, val pat: CompoundPattern, val keyMeta: Meta) extends Ob {
  /* In Rosette this method is called `match` */
  def matchPattern(argvec: Tuple, nargs: Int): Option[Tuple] =
    pat.matchExpr(argvec, nargs)
}

abstract class CompoundPattern(expr: TupleExpr) {
  /* In Rosette this method is called `match` */
  def matchExpr(argvec: Tuple, nargs: Int): Option[Tuple]
}

class IdVecPattern(expr: TupleExpr) extends CompoundPattern(expr) {
  /* In Rosette this method is called `match` */
  override def matchExpr(argvec: Tuple, nargs: Int): Option[Tuple] = {
    val need = expr.numberOfElements()
    if (need == nargs) Some(argvec) else None
  }
}
