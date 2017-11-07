package coop.rchain.rosette

import scala.collection.mutable

case class Code(litvec: Tuple, override val _slot: mutable.Seq[Ob])
    extends Ob {
  def lit(l: Int): Ob = litvec.elem(l)
}
