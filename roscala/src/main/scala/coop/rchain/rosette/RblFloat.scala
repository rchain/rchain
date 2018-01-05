package coop.rchain.rosette

import coop.rchain.rosette.Meta.StdMeta
import coop.rchain.rosette.macros.{checkArgumentMismatch, checkTypeMismatch}
import coop.rchain.rosette.prim.Prim._
import coop.rchain.rosette.prim.{ArithmeticError, Prim, PrimError}

case class RblFloat(value: Double, override val slot: Seq[Ob] = Seq(StdMeta()))
    extends RblAtom {
  override def toString: String = s"Float($value)"

  def +(that: RblFloat) = RblFloat(this.value + that.value)

  def -(that: RblFloat) = RblFloat(this.value - that.value)

  def *(that: RblFloat) = RblFloat(this.value * that.value)

  def /(that: RblFloat) = RblFloat(this.value / that.value)

  def <(that: RblFloat) = RblBool(this.value < that.value)

  def <=(that: RblFloat) = RblBool(this.value <= that.value)

  def >(that: RblFloat) = RblBool(this.value > that.value)

  def >=(that: RblFloat) = RblBool(this.value >= that.value)

  def ==(that: RblFloat) = RblBool(this.value == that.value)

  def !=(that: RblFloat) = RblBool(this.value != that.value)
}

object RblFloat {
  object flPlus extends Prim {
    override val name: String = "fl+"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[RblFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).foldLeft(RblFloat(0.0)) {
        case (accum, float: RblFloat) => accum + float
      })
    }
  }

  object flMinus extends Prim {
    override val name: String = "fl-"
    override val minArgs: Int = 1
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblFloat] =
      ctxt.nargs match {
        case 1 =>
          val n = ctxt.argvec.elem.head.asInstanceOf[RblFloat]
          Right(RblFloat(-n.value))

        case 2 =>
          val m = ctxt.argvec.elem.head.asInstanceOf[RblFloat]
          val n = ctxt.argvec.elem(1).asInstanceOf[RblFloat]
          Right(m - n)
      }
  }

  object flTimes extends Prim {
    override val name: String = "fl*"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[RblFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).foldLeft(RblFloat(1)) {
        case (accum, float: RblFloat) => accum * float
      })
    }
  }

  object flDiv extends Prim {
    override val name: String = "fl/"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RblFloat]
      val n = ctxt.argvec.elem(1).asInstanceOf[RblFloat]

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
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RblFloat]
      val n = ctxt.argvec.elem(1).asInstanceOf[RblFloat]

      Right(m < n)
    }
  }

  object flLe extends Prim {
    override val name: String = "fl<="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RblFloat]
      val n = ctxt.argvec.elem(1).asInstanceOf[RblFloat]

      Right(m <= n)
    }
  }

  object flGt extends Prim {
    override val name: String = "fl>"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RblFloat]
      val n = ctxt.argvec.elem(1).asInstanceOf[RblFloat]

      Right(m > n)
    }
  }

  object flGe extends Prim {
    override val name: String = "fl>="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RblFloat]
      val n = ctxt.argvec.elem(1).asInstanceOf[RblFloat]

      Right(m >= n)
    }
  }

  object flEq extends Prim {
    override val name: String = "fl="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RblFloat]
      val n = ctxt.argvec.elem(1).asInstanceOf[RblFloat]

      Right(m == n)
    }
  }

  object flNe extends Prim {
    override val name: String = "fl!="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RblFloat]
      val n = ctxt.argvec.elem(1).asInstanceOf[RblFloat]

      Right(m != n)
    }
  }

  object flMin extends Prim {
    override val name: String = "fl-min"
    override val minArgs: Int = 1
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[RblFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).foldLeft(RblFloat(Double.MaxValue)) {
        case (minVal, float: RblFloat) =>
          if (minVal.value < float.value) minVal else float
      })
    }
  }

  object flMax extends Prim {
    override val name: String = "fl-max"
    override val minArgs: Int = 1
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[RblFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val n = ctxt.nargs

      Right(ctxt.argvec.elem.take(n).foldLeft(RblFloat(Double.MinValue)) {
        case (maxVal, float: RblFloat) =>
          if (maxVal.value > float.value) maxVal else float
      })
    }
  }

  object flAbs extends Prim {
    override val name: String = "fl-abs"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RblFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RblFloat]

      Right(RblFloat(Math.abs(m.value)))
    }
  }

  object flExp extends Prim {
    override val name: String = "fl-exp"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RblFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RblFloat]

      Right(RblFloat(Math.exp(m.value)))
    }
  }

  object flExpt extends Prim {
    override val name: String = "fl-expt"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RblFloat]
      val n = ctxt.argvec.elem(1).asInstanceOf[RblFloat]

      Right(RblFloat(Math.pow(m.value, n.value)))
    }
  }

  object flLog extends Prim {
    override val name: String = "fl-log"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RblFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RblFloat]

      Right(RblFloat(Math.log(m.value)))
    }
  }

  object flLog10 extends Prim {
    override val name: String = "fl-log10"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RblFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RblFloat]

      Right(RblFloat(Math.log10(m.value)))
    }
  }

  object flFloor extends Prim {
    override val name: String = "fl-floor"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RblFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RblFloat]

      Right(RblFloat(Math.floor(m.value)))
    }
  }

  object flCeil extends Prim {
    override val name: String = "fl-ceil"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RblFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RblFloat]

      Right(RblFloat(Math.ceil(m.value)))
    }
  }

  object flAtan extends Prim {
    override val name: String = "fl-atan"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RblFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RblFloat]

      Right(RblFloat(Math.atan(m.value)))
    }
  }

  object flCos extends Prim {
    override val name: String = "fl-cos"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RblFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RblFloat]

      Right(RblFloat(Math.cos(m.value)))
    }
  }

  object flSin extends Prim {
    override val name: String = "fl-sin"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RblFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblFloat] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RblFloat]

      Right(RblFloat(Math.sin(m.value)))
    }
  }

  object flToFx extends Prim {
    override val name: String = "fl-fx"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RblFloat]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val m = ctxt.argvec.elem.head.asInstanceOf[RblFloat]

      Right(Fixnum(math.floor(m.value).toInt))
    }
  }
}
