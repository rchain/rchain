package coop.rchain.rosette

case class Code(litvec: Tuple,
                override val parent: Ob,
                override val meta: Ob,
                override val slot: Seq[Ob])
    extends Ob {
  def lit(l: Int): Ob = litvec.elem(l)
}
