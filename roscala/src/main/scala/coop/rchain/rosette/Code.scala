package coop.rchain.rosette

case class Code(litvec: Tuple, codevec: Seq[Op]) extends Ob {
  override val meta   = null
  override val parent = null

  def lit(l: Int): Ob = litvec.elem(l)
}
