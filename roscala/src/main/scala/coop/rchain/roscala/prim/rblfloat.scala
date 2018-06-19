package coop.rchain.roscala.prim

import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.ob.{Ctxt, Ob, RblFloat}

object rblfloat {
  object flPlus extends Prim {
    val name    = "fl+"
    val maxArgs = Prim.MaxArgs
    val minArgs = 1
    def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val n = ctxt.nargs

      ctxt.argvec.value.take(n).foldLeft(RblFloat(0)) { (accum, fixnum) =>
        accum + fixnum.asInstanceOf[RblFloat]
      }
    }
  }
}
