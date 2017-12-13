package coop.rchain.rosette

case class Code(litvec: Tuple, codevec: Seq[Op]) extends Ob {
  def lit(l: Int): Ob = litvec.elem(l)
}
