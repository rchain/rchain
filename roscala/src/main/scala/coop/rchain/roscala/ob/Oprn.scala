package coop.rchain.roscala.ob

import com.typesafe.scalalogging.Logger
import Oprn.logger
import coop.rchain.roscala.Vm.State

class Oprn extends Actor {
  override def dispatch(ctxt: Ctxt, state: State): Ob =
    if (state.ctxt.nargs > 0) {
      logger.debug(s"Dispatch ${ctxt.trgt} to ${ctxt.arg(0)}")
      ctxt.arg(0).lookupAndInvoke(ctxt, state)
    } else
      // TODO: Runtime error
      Niv
}

object Oprn {
  val logger = Logger("Oprn")
}
