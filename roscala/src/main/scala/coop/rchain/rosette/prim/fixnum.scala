package coop.rchain.rosette.prim

import coop.rchain.rosette.{Ctxt, Fixnum, Ob, RblBool}
import coop.rchain.rosette.macros.{checkArgumentMismatch, checkTypeMismatch}
import coop.rchain.rosette.prim.Prim._

/**
  * TODO:
  * There is inconsistent behavior between fx<, fx<=, fx>, fx>=, fx=, fx!= and fl<,
  * fl<=, fl>, fl>=, fl=, fl!=
  **/
object fixnum {
  object fxPlus extends Prim {
    override val name: String = "fx+"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).asInstanceOf[Seq[Fixnum]].foldLeft(Fixnum(0)) {
        (accum, fixnum: Fixnum) =>
          accum + fixnum
      })
    }
  }

  object fxMinus extends Prim {
    override val name: String = "fx-"
    override val minArgs: Int = 1
    override val maxArgs: Int = 2

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] =
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
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).asInstanceOf[Seq[Fixnum]].foldLeft(Fixnum(1)) {
        (accum, fixnum: Fixnum) =>
          accum * fixnum
      })
    }
  }

  object fxDiv extends Prim {
    override val name: String = "fx/"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[Fixnum]
      val n = ctxt.argvec.elem(1).asInstanceOf[Fixnum]

      try {
        Right(m / n)
      } catch {
        case _: ArithmeticException =>
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
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[Fixnum]
      val n = ctxt.argvec.elem(1).asInstanceOf[Fixnum]

      try {
        Right(m % n)
      } catch {
        case _: ArithmeticException =>
          Left(ArithmeticError)
      }
    }
  }

  object fxLt extends Prim {
    override val name: String = "fx<"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblBool] =
      checkFixnum(0, ctxt.argvec.elem).map { m =>
        checkFixnum(1, ctxt.argvec.elem) match {
          case Right(n) => m < n
          case Left(_)  => RblBool(false)
        }
      }
  }

  object fxLe extends Prim {
    override val name: String = "fx<="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblBool] =
      checkFixnum(0, ctxt.argvec.elem).map { m =>
        checkFixnum(1, ctxt.argvec.elem) match {
          case Right(n) => m <= n
          case Left(_)  => RblBool(false)
        }
      }
  }

  object fxGt extends Prim {
    override val name: String = "fx>"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblBool] =
      checkFixnum(0, ctxt.argvec.elem).map { m =>
        checkFixnum(1, ctxt.argvec.elem) match {
          case Right(n) => m > n
          case Left(_)  => RblBool(false)
        }
      }
  }

  object fxGe extends Prim {
    override val name: String = "fx>="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblBool] =
      checkFixnum(0, ctxt.argvec.elem).map { m =>
        checkFixnum(1, ctxt.argvec.elem) match {
          case Right(n) => m >= n
          case Left(_)  => RblBool(false)
        }
      }
  }

  object fxEq extends Prim {
    override val name: String = "fx="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblBool] =
      checkFixnum(0, ctxt.argvec.elem).map { m =>
        checkFixnum(1, ctxt.argvec.elem) match {
          case Right(n) => m == n
          case Left(_)  => RblBool(false)
        }
      }
  }

  object fxNe extends Prim {
    override val name: String = "fx!="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblBool] =
      checkFixnum(0, ctxt.argvec.elem).map { m =>
        checkFixnum(1, ctxt.argvec.elem) match {
          case Right(n) => m != n
          case Left(_)  => RblBool(false)
        }
      }
  }

  object fxMin extends Prim {
    override val name: String = "fx-min"
    override val minArgs: Int = 1
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).asInstanceOf[Seq[Fixnum]].foldLeft(Fixnum(Int.MaxValue)) {
        (minVal, fixnum: Fixnum) =>
          if (minVal.value < fixnum.value) minVal else fixnum
      })
    }
  }

  object fxMax extends Prim {
    override val name: String = "fx-max"
    override val minArgs: Int = 1
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).asInstanceOf[Seq[Fixnum]].foldLeft(Fixnum(Int.MinValue)) {
        (maxVal, fixnum: Fixnum) =>
          if (maxVal.value > fixnum.value) maxVal else fixnum
      })
    }
  }

  object fxAbs extends Prim {
    override val name: String = "fx-abs"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[Fixnum]
      Right(Fixnum(Math.abs(m.value)))
    }
  }

  object fxExpt extends Prim {
    override val name: String = "fx-expt"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[Fixnum]
      val n = ctxt.argvec.elem(1).asInstanceOf[Fixnum]

      try {
        Right(Fixnum(Math.pow(m.value.toDouble, n.value.toDouble).toInt))
      } catch {
        case _: ArithmeticException =>
          Left(ArithmeticError)
      }
    }
  }

  object fxLg extends Prim {
    override val name: String = "fx-lg"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[Fixnum].value

      if (m <= 0) {
        Left(ArithmeticError)
      } else {
        try {
          val logn = Math.log(m.toDouble)
          val log2 = Math.log(2.0)

          Right(Fixnum(Math.ceil(logn / log2).toInt))
        } catch {
          case _: ArithmeticException =>
            Left(ArithmeticError)
        }
      }
    }
  }

  object fxLgf extends Prim {
    override val name: String = "fx-lgf"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[Fixnum].value

      if (m <= 0) {
        Left(ArithmeticError)
      } else {
        try {
          val logn = Math.log(m.toDouble)
          val log2 = Math.log(2.0)

          Right(Fixnum(Math.floor(logn / log2).toInt))
        } catch {
          case _: ArithmeticException =>
            Left(ArithmeticError)
        }
      }
    }
  }

  object fxLogand extends Prim {
    override val name: String = "fx-logand"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).asInstanceOf[Seq[Fixnum]].foldLeft(Fixnum(~0)) {
        (accum, fixnum: Fixnum) =>
          accum & fixnum
      })
    }
  }

  object fxLogor extends Prim {
    override val name: String = "fx-logor"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).asInstanceOf[Seq[Fixnum]].foldLeft(Fixnum(0)) {
        (accum, fixnum: Fixnum) =>
          accum | fixnum
      })
    }
  }

  object fxLogxor extends Prim {
    override val name: String = "fx-logxor"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).asInstanceOf[Seq[Fixnum]].foldLeft(Fixnum(0)) {
        (accum, fixnum: Fixnum) =>
          accum ^ fixnum
      })
    }
  }

  object fxLognot extends Prim {
    override val name: String = "fx-lognot"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[Fixnum]

      Right(Fixnum(~m.value))
    }
  }

  object fxMdiv extends Prim {
    override val name: String = "fx-mdiv"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val n = ctxt.nargs

      val commonBits = ctxt.argvec.elem.take(n).asInstanceOf[Seq[Fixnum]].foldLeft(Fixnum(~0)) {
        case (accum, fixnum: Fixnum) => accum & fixnum
      }

      if (commonBits.value != 0) {
        Right(Fixnum(1))
      } else {
        Right(Fixnum(0))
      }
    }
  }

  object fxCdiv extends Prim {
    override val name: String = "fx-cdiv"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[Fixnum]
      val n = ctxt.argvec.elem(1).asInstanceOf[Fixnum]

      try {
        Right(Fixnum(Math.ceil(m.value.toDouble / n.value.toDouble).toInt))
      } catch {
        case _: ArithmeticException =>
          Left(ArithmeticError)
      }
    }
  }

  object fxAsl extends Prim {
    override val name: String = "fx-asl"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[Fixnum]
      val n = ctxt.argvec.elem(1).asInstanceOf[Fixnum]

      Right(m << n)
    }
  }

  object fxAsr extends Prim {
    override val name: String = "fx-asr"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[Fixnum]
      val n = ctxt.argvec.elem(1).asInstanceOf[Fixnum]

      Right(m >> n)
    }
  }

  object fxLsl extends Prim {
    override val name: String = "fx-lsl"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[Fixnum]
      val n = ctxt.argvec.elem(1).asInstanceOf[Fixnum]

      Right(m << n)
    }
  }

  object fxLsr extends Prim {
    override val name: String = "fx-lsr"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[Fixnum]
    @checkArgumentMismatch
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[Fixnum]
      val n = ctxt.argvec.elem(1).asInstanceOf[Fixnum]

      Right(m >>> n)
    }
  }

  private def checkFixnum(n: Int, elem: Seq[Ob]): Either[PrimError, Fixnum] =
    if (!elem(n).isInstanceOf[Fixnum]) {
      Left(TypeMismatch(n, Fixnum.getClass().getName()))
    } else {
      Right(elem(n).asInstanceOf[Fixnum])
    }
}
