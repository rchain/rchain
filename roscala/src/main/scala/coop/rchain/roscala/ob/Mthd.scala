package coop.rchain.roscala.ob

import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.Vm.State

class Mthd(code: Code, id: Ob, source: Ob) extends Ob {
  override def invoke(ctxt: Ctxt, state: State, globalEnv: GlobalEnv): Ob = {
    val surrogate = ctxt.arg(0)
    ctxt.self2 = ctxt.arg(0)
    ctxt.selfEnv = surrogate
    ctxt.env = surrogate
    ctxt.code = this.code
    ctxt.rslt = Niv
    ctxt.pc = 0
    state.strandPool.append(ctxt)
    Suspended
  }

}

object Mthd {
  def apply(code: Code, id: Ob = Qanon, source: Ob = Niv): Mthd =
    new Mthd(code, id, source)
}
