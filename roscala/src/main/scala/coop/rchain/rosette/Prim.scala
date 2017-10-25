package coop.rchain.rosette

case class Prim(override val parent: Ob,
                override val meta: Ob,
                override val slot: Seq[Ob])
    extends Ob {
  def dispatchHelper(ctxt: Ctxt): Either[RblError, Ob] = Right(null)
}

object Prim {
  val nthPrim: Seq[Prim] = new Array[Prim](0)
}
