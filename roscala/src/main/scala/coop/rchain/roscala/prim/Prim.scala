package coop.rchain.roscala.prim

import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob._

abstract class Prim extends Ob {
  def fn(ctxt: Ctxt, globalEnv: GlobalEnv): Ob

  // TODO: Add error case
  def dispatchHelper(state: State, globalEnv: GlobalEnv): Ob = {
    val n = state.ctxt.nargs
    fn(state.ctxt, globalEnv)
  }

  override def dispatch(state: State, globalEnv: GlobalEnv): Ob = {
    val result = dispatchHelper(state, globalEnv)

    if (result != Invalid && result != Upcall && result != Deadthread) {
      state.ctxt.ret(result, state)
    }

    result
  }

  override def invoke(state: State, globalEnv: GlobalEnv): Ob =
    dispatch(state, globalEnv)
}
