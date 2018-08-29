package coop.rchain.roscala.ob

import com.typesafe.scalalogging.Logger
import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob.Mthd.logger

class Mthd(code: Code, id: Ob, source: Ob) extends Ob {
  override def invoke(ctxt: Ctxt, state: State): Ob = {
    logger.debug(s"Schedule $ctxt from $this")

    val surrogate = ctxt.arg(0)
    ctxt.self2 = ctxt.arg(0)
    ctxt.selfEnv = surrogate
    ctxt.env = surrogate
    ctxt.code = this.code
    ctxt.rslt = Niv
    ctxt.pc = 0

    ctxt.scheduleStrand(state)
    Suspended
  }

  override def dispatch(ctxt: Ctxt, state: State): Ob = {

    /**
      * This is the path followed when invoking a "local" method. Because
      * such an invocation doesn't supply the usual "self" argument that a
      * method expects, we have to fix things up just a little bit here.
      */
    val newArgvec = Tuple.cons(ctxt.self2, ctxt.argvec)
    ctxt.code = this.code
    ctxt.env = ctxt.selfEnv
    ctxt.argvec = newArgvec
    ctxt.nargs += 1
    ctxt.rslt = Niv
    ctxt.pc = 0

    ctxt.scheduleStrand(state)
    Suspended
  }

}

object Mthd {
  val logger = Logger("Method")

  def apply(code: Code, id: Ob = Qanon, source: Ob = Niv): Mthd =
    new Mthd(code, id, source)
}
