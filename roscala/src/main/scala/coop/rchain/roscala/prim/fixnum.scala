package coop.rchain.roscala.prim

import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.ob.{Ctxt, Fixnum, Ob, RblBool}
import Prim.MaxArgs
import coop.rchain.roscala.macros.checkTypeMismatch
import coop.rchain.roscala.prim.Prim.mismatchType

/**
  * TODO:
  * There is inconsistent behavior between fx<, fx<=, fx>, fx>=, fx=, fx!= and fl<,
  * fl<=, fl>, fl>=, fl=, fl!=
  */
object fixnum {
  object fxPlus extends Prim {
    def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val n = ctxt.nargs

      ctxt.argvec.value.take(n).foldLeft(Fixnum(0)) { (accum, fixnum) =>
        accum + fixnum.asInstanceOf[Fixnum]
      }
    }
  }

  object fxMinus extends Prim {
    override val name: String = "fx-"
    override val minArgs: Int = 1
    override val maxArgs: Int = 2

    @checkTypeMismatch[Fixnum]
    override def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob =
      ctxt.nargs match {
        case 1 =>
          val fixval = ctxt.argvec.value.head.asInstanceOf[Fixnum].value
          Fixnum(-fixval)

        case 2 =>
          val m = ctxt.argvec.value.head.asInstanceOf[Fixnum].value
          val n = ctxt.argvec.value(1).asInstanceOf[Fixnum].value
          Fixnum(m - n)
      }
  }

  object fxTimes extends Prim {
    override val name: String = "fx*"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[Fixnum]
    override def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val n = ctxt.nargs

      ctxt.argvec.value.take(n).foldLeft(Fixnum(1)) {
        case (accum, fixnum: Fixnum) =>
          accum * fixnum
      }
    }
  }

  object fxDiv extends Prim {
    override val name: String = "fx/"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[Fixnum]
    override def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val m = ctxt.argvec.value.head.asInstanceOf[Fixnum]
      val n = ctxt.argvec.value(1).asInstanceOf[Fixnum]

      try {
        m / n
      } catch {
        case _: ArithmeticException =>
          Prim.runtimeError(ctxt, "arithmetic exception")
      }
    }
  }

  object fxMod extends Prim {
    override val name: String = "fx%"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[Fixnum]
    override def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val m = ctxt.argvec.value.head.asInstanceOf[Fixnum]
      val n = ctxt.argvec.value(1).asInstanceOf[Fixnum]

      try {
        m % n
      } catch {
        case _: ArithmeticException =>
          Prim.runtimeError(ctxt, "arithmetic exception")
      }
    }
  }

  object fxLt extends Prim {
    override val name: String = "fx<"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    override def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob =
      checkFixnum(ctxt, 0, ctxt.argvec.value) match {
        case a: Fixnum =>
          checkFixnum(ctxt, 1, ctxt.argvec.value) match {
            case b: Fixnum => a < b
            case _         => RblBool(false)
          }
        case _ => Prim.runtimeError(ctxt, "arithmetic exception")
      }
  }

  object fxMin extends Prim {
    override val name: String = "fx-min"
    override val minArgs: Int = 1
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[Fixnum]
    override def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val n = ctxt.nargs

      ctxt.argvec.value.take(n).foldLeft(Fixnum(Int.MaxValue)) {
        case (minVal: Fixnum, fixnum: Fixnum) =>
          if (minVal.value < fixnum.value) minVal else fixnum
      }
    }
  }

  object fxMax extends Prim {
    override val name: String = "fx-max"
    override val minArgs: Int = 1
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[Fixnum]
    override def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val n = ctxt.nargs

      ctxt.argvec.value.take(n).foldLeft(Fixnum(Int.MinValue)) {
        case (maxVal, fixnum: Fixnum) =>
          if (maxVal.value > fixnum.value) maxVal else fixnum
      }
    }
  }

  object fxAbs extends Prim {
    override val name: String = "fx-abs"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[Fixnum]
    override def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val m = ctxt.argvec.value.head.asInstanceOf[Fixnum]
      Fixnum(Math.abs(m.value))
    }
  }

  object fxExpt extends Prim {
    override val name: String = "fx-expt"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[Fixnum]
    override def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val m = ctxt.argvec.value.head.asInstanceOf[Fixnum]
      val n = ctxt.argvec.value(1).asInstanceOf[Fixnum]

      try {
        Fixnum(Math.pow(m.value.toDouble, n.value.toDouble).toInt)
      } catch {
        case _: ArithmeticException =>
          Prim.runtimeError(ctxt, "arithmetic exception")
      }
    }
  }

  object fxLg extends Prim {
    override val name: String = "fx-lg"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[Fixnum]
    override def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val m = ctxt.argvec.value.head.asInstanceOf[Fixnum].value

      if (m <= 0) {
        Prim.runtimeError(ctxt, "arithmetic exception")
      } else {
        try {
          val logn = Math.log(m.toDouble)
          val log2 = Math.log(2.0)

          Fixnum(Math.ceil(logn / log2).toInt)
        } catch {
          case _: ArithmeticException =>
            Prim.runtimeError(ctxt, "arithmetic exception")
        }
      }
    }
  }

  object fxLgf extends Prim {
    override val name: String = "fx-lgf"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[Fixnum]
    override def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val m = ctxt.argvec.value.head.asInstanceOf[Fixnum].value

      if (m <= 0) {
        Prim.runtimeError(ctxt, "arithmetic exception")
      } else {
        try {
          val logn = Math.log(m.toDouble)
          val log2 = Math.log(2.0)

          Fixnum(Math.floor(logn / log2).toInt)
        } catch {
          case _: ArithmeticException =>
            Prim.runtimeError(ctxt, "arithmetic exception")
        }
      }
    }
  }

  object fxLogand extends Prim {
    override val name: String = "fx-logand"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[Fixnum]
    override def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val n = ctxt.nargs

      ctxt.argvec.value.take(n).foldLeft(Fixnum(~0)) {
        case (accum, fixnum: Fixnum) => accum & fixnum
      }
    }
  }

  object fxLogor extends Prim {
    override val name: String = "fx-logor"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[Fixnum]
    override def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val n = ctxt.nargs

      ctxt.argvec.value.take(n).foldLeft(Fixnum(0)) {
        case (accum, fixnum: Fixnum) => accum | fixnum
      }
    }
  }

  object fxLogxor extends Prim {
    override val name: String = "fx-logxor"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[Fixnum]
    override def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val n = ctxt.nargs

      ctxt.argvec.value.take(n).foldLeft(Fixnum(0)) {
        case (accum, fixnum: Fixnum) => accum ^ fixnum
      }
    }
  }

  object fxLognot extends Prim {
    override val name: String = "fx-lognot"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[Fixnum]
    override def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val m = ctxt.argvec.value.head.asInstanceOf[Fixnum]

      Fixnum(~m.value)
    }
  }

  object fxMdiv extends Prim {
    override val name: String = "fx-mdiv"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[Fixnum]
    override def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val n = ctxt.nargs

      val commonBits = ctxt.argvec.value.take(n).foldLeft(Fixnum(~0)) {
        case (accum, fixnum: Fixnum) =>
          accum & fixnum
      }

      if (commonBits.value != 0) {
        Fixnum(1)
      } else {
        Fixnum(0)
      }
    }
  }

  object fxCdiv extends Prim {
    override val name: String = "fx-cdiv"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[Fixnum]
    override def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val m = ctxt.argvec.value.head.asInstanceOf[Fixnum]
      val n = ctxt.argvec.value(1).asInstanceOf[Fixnum]

      try {
        Fixnum(Math.ceil(m.value.toDouble / n.value.toDouble).toInt)
      } catch {
        case _: ArithmeticException =>
          Prim.runtimeError(ctxt, "arithmetic exception")
      }
    }
  }

  object fxAsl extends Prim {
    override val name: String = "fx-asl"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[Fixnum]
    override def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val m = ctxt.argvec.value.head.asInstanceOf[Fixnum]
      val n = ctxt.argvec.value(1).asInstanceOf[Fixnum]

      m << n
    }
  }

  object fxAsr extends Prim {
    override val name: String = "fx-asr"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[Fixnum]
    override def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val m = ctxt.argvec.value.head.asInstanceOf[Fixnum]
      val n = ctxt.argvec.value(1).asInstanceOf[Fixnum]

      m >> n
    }
  }

  object fxLsl extends Prim {
    override val name: String = "fx-lsl"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[Fixnum]
    override def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val m = ctxt.argvec.value.head.asInstanceOf[Fixnum]
      val n = ctxt.argvec.value(1).asInstanceOf[Fixnum]

      m << n
    }
  }

  object fxLsr extends Prim {
    override val name: String = "fx-lsr"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[Fixnum]
    override def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val m = ctxt.argvec.value.head.asInstanceOf[Fixnum]
      val n = ctxt.argvec.value(1).asInstanceOf[Fixnum]

      m >>> n
    }
  }

  private def checkFixnum(ctxt: Ctxt, n: Int, elem: Array[Ob]): Ob =
    if (!elem(n).isInstanceOf[Fixnum]) {
      Prim.mismatch(ctxt, n, Fixnum.getClass.getName)
    } else {
      elem(n).asInstanceOf[Fixnum]
    }

}
