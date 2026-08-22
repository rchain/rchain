package coop.rchain.roscala.prim

import coop.rchain.roscala.macros.checkTypeMismatch
import coop.rchain.roscala.ob._
import coop.rchain.roscala.prim.Prim._

/**
  * TODO:
  * There is inconsistent behavior between fx<, fx<=, fx>, fx>=, fx=, fx!= and fl<,
  * fl<=, fl>, fl>=, fl=, fl!=
  **/
object rblfloat {
  object flPlus extends Prim {
    override val name: String = "fl+"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[RblFloat]
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.value.take(n).foldLeft(RblFloat(0.0)) { (accum, float) =>
        accum + float.asInstanceOf[RblFloat]
      })
    }
  }

  object flMinus extends Prim {
    override val name: String = "fl-"
    override val minArgs: Int = 1
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblFloat]
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblFloat] =
      ctxt.nargs match {
        case 1 =>
          val n = ctxt.argvec.value.head.asInstanceOf[RblFloat]
          Right(-n)

        case 2 =>
          val m = ctxt.argvec.value.head.asInstanceOf[RblFloat]
          val n = ctxt.argvec.value(1).asInstanceOf[RblFloat]
          Right(m - n)
      }
  }

  object flTimes extends Prim {
    override val name: String = "fl*"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[RblFloat]
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.value.take(n).foldLeft(RblFloat(1)) { (accum, float) =>
        accum * float.asInstanceOf[RblFloat]
      })
    }
  }

  object flDiv extends Prim {
    override val name: String = "fl/"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblFloat]
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val m = ctxt.argvec.value.head.asInstanceOf[RblFloat]
      val n = ctxt.argvec.value(1).asInstanceOf[RblFloat]

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

    @checkTypeMismatch[RblFloat]
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val m = ctxt.argvec.value.head.asInstanceOf[RblFloat]
      val n = ctxt.argvec.value(1).asInstanceOf[RblFloat]

      Right(m < n)
    }
  }

  object flLe extends Prim {
    override val name: String = "fl<="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblFloat]
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val m = ctxt.argvec.value.head.asInstanceOf[RblFloat]
      val n = ctxt.argvec.value(1).asInstanceOf[RblFloat]

      Right(m <= n)
    }
  }

  object flGt extends Prim {
    override val name: String = "fl>"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblFloat]
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val m = ctxt.argvec.value.head.asInstanceOf[RblFloat]
      val n = ctxt.argvec.value(1).asInstanceOf[RblFloat]

      Right(m > n)
    }
  }

  object flGe extends Prim {
    override val name: String = "fl>="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblFloat]
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val m = ctxt.argvec.value.head.asInstanceOf[RblFloat]
      val n = ctxt.argvec.value(1).asInstanceOf[RblFloat]

      Right(m >= n)
    }
  }

  object flEq extends Prim {
    override val name: String = "fl="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblFloat]
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val m = ctxt.argvec.value.head.asInstanceOf[RblFloat]
      val n = ctxt.argvec.value(1).asInstanceOf[RblFloat]

      Right(m == n)
    }
  }

  object flNe extends Prim {
    override val name: String = "fl!="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblFloat]
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val m = ctxt.argvec.value.head.asInstanceOf[RblFloat]
      val n = ctxt.argvec.value(1).asInstanceOf[RblFloat]

      Right(m != n)
    }
  }

  object flMin extends Prim {
    override val name: String = "fl-min"
    override val minArgs: Int = 1
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[RblFloat]
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.value.take(n).foldLeft(RblFloat(Double.MaxValue)) { (minVal, float) =>
        if (minVal.value < float.asInstanceOf[RblFloat].value) minVal
        else float.asInstanceOf[RblFloat]
      })
    }
  }

  object flMax extends Prim {
    override val name: String = "fl-max"
    override val minArgs: Int = 1
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[RblFloat]
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.value.take(n).foldLeft(RblFloat(Double.MinValue)) { (maxVal, float) =>
        if (maxVal.value > float.asInstanceOf[RblFloat].value) maxVal
        else float.asInstanceOf[RblFloat]
      })
    }
  }

  object flAbs extends Prim {
    override val name: String = "fl-abs"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RblFloat]
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val m = ctxt.argvec.value.head.asInstanceOf[RblFloat]

      Right(RblFloat(Math.abs(m.value)))
    }
  }

  object flExp extends Prim {
    override val name: String = "fl-exp"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RblFloat]
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val m = ctxt.argvec.value.head.asInstanceOf[RblFloat]

      Right(RblFloat(Math.exp(m.value)))
    }
  }

  object flExpt extends Prim {
    override val name: String = "fl-expt"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblFloat]
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val m = ctxt.argvec.value.head.asInstanceOf[RblFloat]
      val n = ctxt.argvec.value(1).asInstanceOf[RblFloat]

      Right(RblFloat(Math.pow(m.value, n.value)))
    }
  }

  object flLog extends Prim {
    override val name: String = "fl-log"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RblFloat]
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val m = ctxt.argvec.value.head.asInstanceOf[RblFloat]

      Right(RblFloat(Math.log(m.value)))
    }
  }

  object flLog10 extends Prim {
    override val name: String = "fl-log10"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RblFloat]
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val m = ctxt.argvec.value.head.asInstanceOf[RblFloat]

      Right(RblFloat(Math.log10(m.value)))
    }
  }

  object flFloor extends Prim {
    override val name: String = "fl-floor"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RblFloat]
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val m = ctxt.argvec.value.head.asInstanceOf[RblFloat]

      Right(RblFloat(Math.floor(m.value)))
    }
  }

  object flCeil extends Prim {
    override val name: String = "fl-ceil"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RblFloat]
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val m = ctxt.argvec.value.head.asInstanceOf[RblFloat]

      Right(RblFloat(Math.ceil(m.value)))
    }
  }

  object flAtan extends Prim {
    override val name: String = "fl-atan"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RblFloat]
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val m = ctxt.argvec.value.head.asInstanceOf[RblFloat]

      Right(RblFloat(Math.atan(m.value)))
    }
  }

  object flCos extends Prim {
    override val name: String = "fl-cos"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RblFloat]
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val m = ctxt.argvec.value.head.asInstanceOf[RblFloat]

      Right(RblFloat(Math.cos(m.value)))
    }
  }

  object flSin extends Prim {
    override val name: String = "fl-sin"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RblFloat]
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val m = ctxt.argvec.value.head.asInstanceOf[RblFloat]

      Right(RblFloat(Math.sin(m.value)))
    }
  }

  object flToFx extends Prim {
    override val name: String = "fl-fx"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RblFloat]
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val m = ctxt.argvec.value.head.asInstanceOf[RblFloat]

      Right(Fixnum(math.floor(m.value).toInt))
    }
  }
}
