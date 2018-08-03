package coop.rchain.roscala.prim

import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob.{Ctxt, Ob}
import coop.rchain.roscala.prim.Prim._

object actor {
  object actorUpdateBang extends Prim {
    override val name: String = "update!"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    override def fn(ctxt: Ctxt, state: State): Ob =
      ctxt.self2.update(enabledSetProvided = false, ctxt, state)

    override def fnSimple(ctxt: Ctxt): Either[PrimError, Ob] = ???
  }
}
