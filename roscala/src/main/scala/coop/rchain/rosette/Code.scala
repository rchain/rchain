package coop.rchain.rosette

import scala.collection.mutable

case class Code(litvec: Tuple,
                codevec: Seq[Op],
                override val _slot: mutable.Seq[Ob])
    extends Ob {
  def lit(l: Int): Ob = litvec.elem(l)
}
