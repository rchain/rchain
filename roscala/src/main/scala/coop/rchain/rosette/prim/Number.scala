package coop.rchain.rosette.prim
import coop.rchain.rosette.macros.{checkArgumentMismatch, checkTypeMismatch}
import coop.rchain.rosette.{Ctxt, Fixnum}
import coop.rchain.rosette.prim.Prim._

object Number {
  object fxPlus extends Prim {
    override val name: String = "fx+"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).foldLeft(Fixnum(0)) {
        case (accum, fixnum: Fixnum) => accum + fixnum
      })
    }
  }

  object fxMinus extends Prim {
    override val name: String = "fx-"
    override val minArgs: Int = 1
    override val maxArgs: Int = 2

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Fixnum] =
      ctxt.nargs match {
        case 1 =>
          val fixval = ctxt.argvec.elem.head.asInstanceOf[Fixnum].value
          Right(Fixnum(-fixval))

        case 2 =>
          val m = ctxt.argvec.elem.head.asInstanceOf[Fixnum].value
          val n = ctxt.argvec.elem(1).asInstanceOf[Fixnum].value
          Right(Fixnum(m - n))
      }
  }

  object fxTimes extends Prim {
    override val name: String = "fx*"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).foldLeft(Fixnum(1)) {
        case (accum, fixnum: Fixnum) => accum * fixnum
      })
    }
  }

  object fxDiv extends Prim {
    override val name: String = "fx/"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[Fixnum].value
      val n = ctxt.argvec.elem(1).asInstanceOf[Fixnum].value

      try {
        Right(Fixnum(m / n))
      } catch {
        case e: ArithmeticException =>
          Left(ArithmeticError)
      }
    }
  }

  object fxMod extends Prim {
    override val name: String = "fx%"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[Fixnum].value
      val n = ctxt.argvec.elem(1).asInstanceOf[Fixnum].value

      try {
        Right(Fixnum(m % n))
      } catch {
        case e: ArithmeticException =>
          Left(ArithmeticError)
      }
    }
  }
}
