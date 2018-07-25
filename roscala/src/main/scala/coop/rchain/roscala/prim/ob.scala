package coop.rchain.roscala.prim

import coop.rchain.roscala.ob.{Ctxt, Fixnum}

object ob {

  /**
    * Define the tuple-cons primitive.
    * This prepends an Ob to the specified Tuple
    * e.g. (tuple-cons 1 [2 3]) ==> [1 2 3]
    */
  object objectIndexedSize extends Prim {
    override val name: String = "prim-size"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] =
      Right(ctxt.arg(0).indexedSize())
  }

}
