package coop.rchain.rosette.prim

import coop.rchain.rosette.{Ctxt, Ob, Tuple}
import coop.rchain.rosette.macros.{checkArgumentMismatch, checkTypeMismatch}
import coop.rchain.rosette.prim.Prim._

object tuple {

  /**
    * Define the tuple-cons primitive.
    * This prepends an Ob to the specified Tuple
    * e.g. (tuple-cons 1 [2 3]) ==> [1 2 3]
    */
  object tplCons extends Prim {
    override val name: String = "tuple-cons"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Tuple] = {
      val elem = ctxt.argvec.elem
      checkTuple(1, elem).map(Tuple.cons(elem.head, _)) // arg1 must be a Tuple
    }
  }

  /**
    * Define the tuple-cons* primitive.
    * This prepends n Obs to the specified Tuple
    * e.g. (tuple-cons* 1 2 3 4 [5 6]) ==> [1 2 3 4 5 6]
    */
  object tplConsStar extends Prim {
    override val name: String = "tuple-cons*"
    override val minArgs: Int = 1
    override val maxArgs: Int = MaxArgs

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Tuple] = {
      val elem = ctxt.argvec.elem
      val nargs = ctxt.nargs
      val last = nargs - 1
      val slice = elem.slice(0, last)

      checkTuple(last, elem)
        .map(Tuple(Tuple(slice), _)) // last arg must be a Tuple
    }
  }

  /**
    * Define the "tuple-rcons" primitive.
    * This appends an Ob to the specified Tuple
    * e.g. (tuple-rcons [1 2] 3) ==> [1 2 3]
    */
  object tplRcons extends Prim {
    override val name: String = "tuple-rcons"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Tuple] = {
      val elem = ctxt.argvec.elem

      checkTuple(0, elem).map(Tuple.rcons(_, elem(1))) // arg0 must be a Tuple
    }
  }

  /**
    * Define the tuple-concat primitive.
    * This concatenates n Tuples
    * e.g. (tuple-concat [1 2 3] [4] [5 6]) ==> [1 2 3 4 5 6]
    */
  object tplConcat extends Prim {
    override val name: String = "tuple-concat"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[Tuple] // All args must be Tuples
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Tuple] = {
      val elem = ctxt.argvec.elem
      val n = ctxt.nargs
      var ret = Tuple(Seq.empty)

      for (el <- elem) {
        ret = Tuple(ret, el.asInstanceOf[Tuple])
      }

      Right(ret)
    }
  }

  /** Helper functions begin here */
  /**
    * Check the specified parameter for type Tuple. Return a PrimError if it is
    * not else return the Tuple.
    */
  private def checkTuple(n: Int, elem: Seq[Ob]): Either[PrimError, Tuple] =
    if (!elem(n).isInstanceOf[Tuple]) {
      Left(TypeMismatch(n, Tuple.getClass.getName))
    } else {
      Right(elem(n).asInstanceOf[Tuple])
    }
}
