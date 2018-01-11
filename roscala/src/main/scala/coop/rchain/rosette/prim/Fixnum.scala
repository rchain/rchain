package coop.rchain.rosette.prim

import coop.rchain.rosette.{Ctxt, Fixnum => RFixnum, Ob, RblBool}
import coop.rchain.rosette.macros.{checkArgumentMismatch, checkTypeMismatch}
import coop.rchain.rosette.prim.Prim._

/**
  * TODO:
  * There is inconsistent behavior between fx<, fx<=, fx>, fx>=, fx=, fx!= and fl<,
  * fl<=, fl>, fl>=, fl=, fl!=
  **/
object Fixnum {
  object fxPlus extends Prim {
    override val name: String = "fx+"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[RFixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFixnum] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).foldLeft(RFixnum(0)) {
        case (accum, fixnum: RFixnum) => accum + fixnum
      })
    }
  }

  object fxMinus extends Prim {
    override val name: String = "fx-"
    override val minArgs: Int = 1
    override val maxArgs: Int = 2

    @checkTypeMismatch[RFixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFixnum] =
      ctxt.nargs match {
        case 1 =>
          val fixval = ctxt.argvec.elem.head.asInstanceOf[RFixnum].value
          Right(RFixnum(-fixval))

        case 2 =>
          val m = ctxt.argvec.elem.head.asInstanceOf[RFixnum].value
          val n = ctxt.argvec.elem(1).asInstanceOf[RFixnum].value
          Right(RFixnum(m - n))
      }
  }

  object fxTimes extends Prim {
    override val name: String = "fx*"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[RFixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFixnum] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).foldLeft(RFixnum(1)) {
        case (accum, fixnum: RFixnum) => accum * fixnum
      })
    }
  }

  object fxDiv extends Prim {
    override val name: String = "fx/"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RFixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFixnum]
      val n = ctxt.argvec.elem(1).asInstanceOf[RFixnum]

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

    @checkTypeMismatch[RFixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFixnum]
      val n = ctxt.argvec.elem(1).asInstanceOf[RFixnum]

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
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] =
      checkRFixnum(0, ctxt.argvec.elem).map { m =>
        checkRFixnum(1, ctxt.argvec.elem) match {
          case Right(n) => m < n
          case Left(_) => RblBool(false)
        }
      }
  }

  object fxLe extends Prim {
    override val name: String = "fx<="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] =
      checkRFixnum(0, ctxt.argvec.elem).map { m =>
        checkRFixnum(1, ctxt.argvec.elem) match {
          case Right(n) => m <= n
          case Left(_) => RblBool(false)
        }
      }
  }

  object fxGt extends Prim {
    override val name: String = "fx>"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] =
      checkRFixnum(0, ctxt.argvec.elem).map { m =>
        checkRFixnum(1, ctxt.argvec.elem) match {
          case Right(n) => m > n
          case Left(_) => RblBool(false)
        }
      }
  }

  object fxGe extends Prim {
    override val name: String = "fx>="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] =
      checkRFixnum(0, ctxt.argvec.elem).map { m =>
        checkRFixnum(1, ctxt.argvec.elem) match {
          case Right(n) => m >= n
          case Left(_) => RblBool(false)
        }
      }
  }

  object fxEq extends Prim {
    override val name: String = "fx="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] =
      checkRFixnum(0, ctxt.argvec.elem).map { m =>
        checkRFixnum(1, ctxt.argvec.elem) match {
          case Right(n) => m == n
          case Left(_) => RblBool(false)
        }
      }
  }

  object fxNe extends Prim {
    override val name: String = "fx!="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] =
      checkRFixnum(0, ctxt.argvec.elem).map { m =>
        checkRFixnum(1, ctxt.argvec.elem) match {
          case Right(n) => m != n
          case Left(_) => RblBool(false)
        }
      }
  }

  object fxMin extends Prim {
    override val name: String = "fx-min"
    override val minArgs: Int = 1
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[RFixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFixnum] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).foldLeft(RFixnum(Int.MaxValue)) {
        case (minVal, fixnum: RFixnum) =>
          if (minVal.value < fixnum.value) minVal else fixnum
      })
    }
  }

  object fxMax extends Prim {
    override val name: String = "fx-max"
    override val minArgs: Int = 1
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[RFixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFixnum] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).foldLeft(RFixnum(Int.MinValue)) {
        case (maxVal, fixnum: RFixnum) =>
          if (maxVal.value > fixnum.value) maxVal else fixnum
      })
    }
  }

  object fxAbs extends Prim {
    override val name: String = "fx-abs"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RFixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFixnum]
      Right(RFixnum(Math.abs(m.value)))
    }
  }

  object fxExpt extends Prim {
    override val name: String = "fx-expt"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RFixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFixnum]
      val n = ctxt.argvec.elem(1).asInstanceOf[RFixnum]

      try {
        Right(RFixnum(Math.pow(m.value, n.value).toInt))
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

    @checkTypeMismatch[RFixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFixnum].value

      if (m <= 0) {
        Left(ArithmeticError)
      } else {
        try {
          val logn = Math.log(m.toDouble)
          val log2 = Math.log(2.0)

          Right(RFixnum(Math.ceil(logn / log2).toInt))
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

    @checkTypeMismatch[RFixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFixnum].value

      if (m <= 0) {
        Left(ArithmeticError)
      } else {
        try {
          val logn = Math.log(m.toDouble)
          val log2 = Math.log(2.0)

          Right(RFixnum(Math.floor(logn / log2).toInt))
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

    @checkTypeMismatch[RFixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFixnum] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).foldLeft(RFixnum(~0)) {
        case (accum, fixnum: RFixnum) => accum & fixnum
      })
    }
  }

  object fxLogor extends Prim {
    override val name: String = "fx-logor"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[RFixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFixnum] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).foldLeft(RFixnum(0)) {
        case (accum, fixnum: RFixnum) => accum | fixnum
      })
    }
  }

  object fxLogxor extends Prim {
    override val name: String = "fx-logxor"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[RFixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFixnum] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).foldLeft(RFixnum(0)) {
        case (accum, fixnum: RFixnum) => accum ^ fixnum
      })
    }
  }

  object fxLognot extends Prim {
    override val name: String = "fx-lognot"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RFixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFixnum]

      Right(RFixnum(~m.value))
    }
  }

  object fxMdiv extends Prim {
    override val name: String = "fx-mdiv"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[RFixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFixnum] = {
      val n = ctxt.nargs

      val commonBits = ctxt.argvec.elem.take(n).foldLeft(RFixnum(~0)) {
        case (accum, fixnum: RFixnum) => accum & fixnum
      }

      if (commonBits.value != 0) {
        Right(RFixnum(1))
      } else {
        Right(RFixnum(0))
      }
    }
  }

  object fxCdiv extends Prim {
    override val name: String = "fx-cdiv"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RFixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFixnum]
      val n = ctxt.argvec.elem(1).asInstanceOf[RFixnum]

      try {
        Right(RFixnum(Math.ceil(m.value.toDouble / n.value.toDouble).toInt))
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

    @checkTypeMismatch[RFixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFixnum]
      val n = ctxt.argvec.elem(1).asInstanceOf[RFixnum]

      Right(m << n)
    }
  }

  object fxAsr extends Prim {
    override val name: String = "fx-asr"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RFixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFixnum]
      val n = ctxt.argvec.elem(1).asInstanceOf[RFixnum]

      Right(m >> n)
    }
  }

  object fxLsl extends Prim {
    override val name: String = "fx-lsl"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RFixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFixnum]
      val n = ctxt.argvec.elem(1).asInstanceOf[RFixnum]

      Right(m << n)
    }
  }

  object fxLsr extends Prim {
    override val name: String = "fx-lsr"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RFixnum]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFixnum]
      val n = ctxt.argvec.elem(1).asInstanceOf[RFixnum]

      Right(m >>> n)
    }
  }

  private def checkRFixnum(n: Int, elem: Seq[Ob]): Either[PrimError, RFixnum] =
    if (!elem(n).isInstanceOf[RFixnum]) {
      Left(TypeMismatch(n, RFixnum.getClass().getName()))
    } else {
      Right(elem(n).asInstanceOf[RFixnum])
    }
}
