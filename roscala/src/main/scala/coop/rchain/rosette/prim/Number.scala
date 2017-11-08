package coop.rchain.rosette.prim
import coop.rchain.rosette.{Ctxt, Fixnum, PrimMismatch}

object Number {
  object fxPlus extends Prim {
    override val externalName: String = "fx+"
    override val minArgs: Int = 0
    override val maxArgs: Int = 255

    override def fn(ctxt: Ctxt): Either[PrimMismatch, Fixnum] = {
      val n = ctxt.nargs
      val isArgsFixnum =
        ctxt.argvec.elem.take(n).forall(_.isInstanceOf[Fixnum])

      if (isArgsFixnum) {
        Right(ctxt.argvec.elem.take(n).foldLeft(Fixnum(0)) {
          case (accum, fixnum: Fixnum) => accum + fixnum
        })
      } else {
        Left(PrimMismatch("no fixnum arguments"))
      }
    }
  }
}
