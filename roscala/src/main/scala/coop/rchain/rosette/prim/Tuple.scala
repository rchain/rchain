package coop.rchain.rosette.prim

import coop.rchain.rosette.{
  Ctxt,
  Fixnum => RFixnum,
  Ob,
  RblBool,
  Tuple => RTuple
}
import coop.rchain.rosette.macros.{checkArgumentMismatch, checkTypeMismatch}
import coop.rchain.rosette.prim.Prim._

//DEF("tuple-cons", tplCons, 2, 2) {
//    CHECK(1, Tuple, tail);
//    return cons(ARG(0), tail);
//}

object Tuple {
  object tplCons extends Prim {
    override val name: String = "tuple-cons"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    //@checkTypeMismatch[RTuple]    arg0 can be any Ob
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RTuple] = {
      val elem = ctxt.argvec.elem
      checkRTuple(1, elem).map { tup => // arg1 must be a Tuple
        RTuple.cons(elem(0), tup)
      }
    }

  }

  private def checkRTuple(n: Int, elem: Seq[Ob]): Either[PrimError, RTuple] =
    if (!elem(n).isInstanceOf[RTuple]) {
      Left(TypeMismatch(n, RTuple.getClass().getName()))
    } else {
      Right(elem(n).asInstanceOf[RTuple])
    }

}
