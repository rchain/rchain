package coop.rchain.roscala.prim

import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.ob.{Ctxt, Fixnum, Ob}

object fixnum {
  object fxPlus extends Prim {
    def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val n = ctxt.nargs

      ctxt.argvec.value.take(n).foldLeft(Fixnum(0)) { (accum, fixnum) =>
        accum + fixnum.asInstanceOf[Fixnum]
      }
    }
  }
}
