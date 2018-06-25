package coop.rchain.roscala.ob

import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.Vm.State

class MboxOb extends Ob {
  var mbox: Ob = MboxOb.LockedMbox

  def receive(ctxt: Ctxt, state: State, globalEnv: GlobalEnv): Ob = {
    ctxt.rcvr = this
    mbox.receiveMsg(this, ctxt, state, globalEnv)
  }

  def schedule(ctxt: Ctxt, state: State, globalEnv: GlobalEnv): Unit =
    state.strandPool.append(ctxt)
}

object MboxOb {
  val LockedMbox: Ob = Invalid
}

class EmptyMbox extends Ob {
  override def receiveMsg(client: MboxOb, task: Ctxt, state: State, globalEnv: GlobalEnv): Ob = {
    client.mbox = MboxOb.LockedMbox
    client.schedule(task, state, globalEnv)
    Niv
  }
}
