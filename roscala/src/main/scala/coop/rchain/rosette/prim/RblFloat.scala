package coop.rchain.rosette.prim

import coop.rchain.rosette.{
  Ctxt,
  Fixnum => RFixnum,
  RblBool,
  RblFloat => RFloat
}
import coop.rchain.rosette.macros.{checkArgumentMismatch, checkTypeMismatch}
import coop.rchain.rosette.prim.Prim._

/**
  * TODO:
  * There is inconsistent behavior between fx<, fx<=, fx>, fx>=, fx=, fx!= and fl<,
  * fl<=, fl>, fl>=, fl=, fl!=
  **/
object RblFloat {
  object flPlus extends Prim {
    override val name: String = "fl+"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[RFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFloat] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).foldLeft(RFloat(0.0)) {
        case (accum, float: RFloat) => accum + float
      })
    }
  }

  object flMinus extends Prim {
    override val name: String = "fl-"
    override val minArgs: Int = 1
    override val maxArgs: Int = 2

    @checkTypeMismatch[RFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFloat] =
      ctxt.nargs match {
        case 1 =>
          val n = ctxt.argvec.elem.head.asInstanceOf[RFloat]
          Right(RFloat(-n.value))

        case 2 =>
          val m = ctxt.argvec.elem.head.asInstanceOf[RFloat]
          val n = ctxt.argvec.elem(1).asInstanceOf[RFloat]
          Right(m - n)
      }
  }

  object flTimes extends Prim {
    override val name: String = "fl*"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[RFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFloat] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).foldLeft(RFloat(1)) {
        case (accum, float: RFloat) => accum * float
      })
    }
  }

  object flDiv extends Prim {
    override val name: String = "fl/"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFloat] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFloat]
      val n = ctxt.argvec.elem(1).asInstanceOf[RFloat]

      try {
        Right(m / n)
      } catch {
        case _: ArithmeticException =>
          Left(ArithmeticError)
      }
    }
  }

  object flLt extends Prim {
    override val name: String = "fl<"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFloat]
      val n = ctxt.argvec.elem(1).asInstanceOf[RFloat]

      Right(m < n)
    }
  }

  object flLe extends Prim {
    override val name: String = "fl<="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFloat]
      val n = ctxt.argvec.elem(1).asInstanceOf[RFloat]

      Right(m <= n)
    }
  }

  object flGt extends Prim {
    override val name: String = "fl>"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFloat]
      val n = ctxt.argvec.elem(1).asInstanceOf[RFloat]

      Right(m > n)
    }
  }

  object flGe extends Prim {
    override val name: String = "fl>="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFloat]
      val n = ctxt.argvec.elem(1).asInstanceOf[RFloat]

      Right(m >= n)
    }
  }

  object flEq extends Prim {
    override val name: String = "fl="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFloat]
      val n = ctxt.argvec.elem(1).asInstanceOf[RFloat]

      Right(m == n)
    }
  }

  object flNe extends Prim {
    override val name: String = "fl!="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFloat]
      val n = ctxt.argvec.elem(1).asInstanceOf[RFloat]

      Right(m != n)
    }
  }

  object flMin extends Prim {
    override val name: String = "fl-min"
    override val minArgs: Int = 1
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[RFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFloat] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).foldLeft(RFloat(Double.MaxValue)) {
        case (minVal, float: RFloat) =>
          if (minVal.value < float.value) minVal else float
      })
    }
  }

  object flMax extends Prim {
    override val name: String = "fl-max"
    override val minArgs: Int = 1
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[RFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFloat] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).foldLeft(RFloat(Double.MinValue)) {
        case (maxVal, float: RFloat) =>
          if (maxVal.value > float.value) maxVal else float
      })
    }
  }

  object flAbs extends Prim {
    override val name: String = "fl-abs"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFloat] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFloat]

      Right(RFloat(Math.abs(m.value)))
    }
  }

  object flExp extends Prim {
    override val name: String = "fl-exp"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFloat] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFloat]

      Right(RFloat(Math.exp(m.value)))
    }
  }

  object flExpt extends Prim {
    override val name: String = "fl-expt"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFloat] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFloat]
      val n = ctxt.argvec.elem(1).asInstanceOf[RFloat]

      Right(RFloat(Math.pow(m.value, n.value)))
    }
  }

  object flLog extends Prim {
    override val name: String = "fl-log"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFloat] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFloat]

      Right(RFloat(Math.log(m.value)))
    }
  }

  object flLog10 extends Prim {
    override val name: String = "fl-log10"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFloat] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFloat]

      Right(RFloat(Math.log10(m.value)))
    }
  }

  object flFloor extends Prim {
    override val name: String = "fl-floor"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFloat] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFloat]

      Right(RFloat(Math.floor(m.value)))
    }
  }

  object flCeil extends Prim {
    override val name: String = "fl-ceil"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFloat] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFloat]

      Right(RFloat(Math.ceil(m.value)))
    }
  }

  object flAtan extends Prim {
    override val name: String = "fl-atan"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFloat] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFloat]

      Right(RFloat(Math.atan(m.value)))
    }
  }

  object flCos extends Prim {
    override val name: String = "fl-cos"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFloat] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFloat]

      Right(RFloat(Math.cos(m.value)))
    }
  }

  object flSin extends Prim {
    override val name: String = "fl-sin"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFloat] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFloat]

      Right(RFloat(Math.sin(m.value)))
    }
  }

  object flToFx extends Prim {
    override val name: String = "fl-fx"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RFixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RFloat]

      Right(RFixnum(math.floor(m.value).toInt))
    }
  }
}
