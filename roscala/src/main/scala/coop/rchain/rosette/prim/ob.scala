package coop.rchain.rosette.prim

import coop.rchain.rosette.{Ctxt, Ob, RblString, RblSymbol}
import coop.rchain.rosette.macros.{checkArgumentMismatch, checkTypeMismatch}
import coop.rchain.rosette.prim.Prim._

object ob {

  /**
    * Define the object->string primitive.
    * Returns the string representation of the supplied arguments.
    * e.g. (object->string 1)   ==> "1"
    *      (object->string fx+) ==> "{Prim}"
    */
  object objectToString extends Prim {
    override val name: String = "object->string"
    override val minArgs: Int = 1
    override val maxArgs: Int = MaxArgs

    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblString] = {
      val elem = ctxt.argvec.elem
      val n    = ctxt.nargs
      val init = ""

      Right(
        RblString(elem.foldLeft(init)((acc: String, el: Ob) => (acc ++ el.toString)))
      )
    }
  }

  /**
    * Define the object->symbol primitive.
    * Returns the symbol representation of the supplied arguments.
    * e.g. (object->symbol 1)   ==> '1
    *      (object->symbol fx+) ==> '{Prim}
    */
  object objectToSymbol extends Prim {
    override val name: String = "object->symbol"
    override val minArgs: Int = 1
    override val maxArgs: Int = MaxArgs

    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblSymbol] = {
      val elem = ctxt.argvec.elem
      val n    = ctxt.nargs
      val init = ""

      Right(
        RblSymbol(Symbol(elem.foldLeft(init)((acc: String, el: Ob) => (acc ++ el.toString))))
      )
    }
  }

}
