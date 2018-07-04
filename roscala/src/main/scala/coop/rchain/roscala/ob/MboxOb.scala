package coop.rchain.roscala.ob

import coop.rchain.roscala.Vm.State

class MboxOb extends Ob {
  var mbox: Ob = MboxOb.LockedMbox

  def receive(ctxt: Ctxt, state: State): Ob = {
    ctxt.rcvr = this
    mbox.receiveMsg(this, ctxt, state)
  }

  def schedule(ctxt: Ctxt, state: State): Unit =
    state.strandPool.enqueue((ctxt, state.globalEnv))
}

object MboxOb {
  val LockedMbox: Ob = Invalid
}

class EmptyMbox extends Ob {
  override def receiveMsg(client: MboxOb, task: Ctxt, state: State): Ob = {
    client.mbox = MboxOb.LockedMbox
    client.schedule(task, state)
    Niv
  }
}
