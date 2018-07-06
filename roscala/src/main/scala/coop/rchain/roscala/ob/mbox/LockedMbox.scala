package coop.rchain.roscala.ob.mbox

import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob.{Ctxt, Nil, Niv, Ob}

class LockedMbox extends Ob {
  override def receiveMsg(client: MboxOb, task: Ctxt, state: State, globalEnv: GlobalEnv): Ob = {
    MboxOb.logger.debug("Locked mailbox receives message")

    val newMbox = QueueMbox(Nil)
    newMbox.enqueue(task)
    client.mbox = newMbox
    Niv
  }
}
