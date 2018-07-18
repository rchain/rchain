package coop.rchain.roscala.ob

import com.typesafe.scalalogging.Logger
import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob.Mthd.logger

class Mthd(code: Code, id: Ob, source: Ob) extends Ob {
  override def invoke(ctxt: Ctxt, state: State, globalEnv: GlobalEnv): Ob = {
    logger.debug(s"Schedule $ctxt from $this")

    val surrogate = ctxt.arg(0)
    ctxt.self2 = ctxt.arg(0)
    ctxt.selfEnv = surrogate
    ctxt.env = surrogate
    ctxt.code = this.code
    ctxt.rslt = Niv
    ctxt.pc = 0

    state.strandPool.prepend(ctxt)
    Suspended
  }

}

object Mthd {
  val logger = Logger("Method")

  def apply(code: Code, id: Ob = Qanon, source: Ob = Niv): Mthd =
    new Mthd(code, id, source)
}
