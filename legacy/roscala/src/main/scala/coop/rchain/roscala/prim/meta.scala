package coop.rchain.roscala.prim

import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.ob.{Ctxt, Meta, Ob}

object meta {
  object addObo {
    def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val meta   = ctxt.argvec(0).asInstanceOf[Meta]
      val client = ctxt.argvec(1)
      val key    = ctxt.argvec(2)
      val value  = ctxt.argvec(3)

      meta.add(client, key, value, ctxt)(globalEnv)
    }
  }

  object getObo {
    def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob = {
      val meta   = ctxt.argvec(0).asInstanceOf[Meta]
      val client = ctxt.argvec(1)
      val key    = ctxt.argvec(2)

      meta.get(client, key, globalEnv)
    }
  }
}
