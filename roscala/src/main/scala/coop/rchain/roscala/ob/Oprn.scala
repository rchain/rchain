package coop.rchain.roscala.ob

import com.typesafe.scalalogging.Logger
import Oprn.logger
import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.Vm.State

case class Oprn() extends Actor {
  override def dispatch(state: State, globalEnv: GlobalEnv): Ob =
    if (state.ctxt.nargs > 0) {
      logger.debug("Dispatch")
      state.ctxt.arg(0).lookupAndInvoke(state, globalEnv)
    } else
      // TODO: Runtime error
      Niv
}

object Oprn {
  val logger = Logger("Oprn")
}
