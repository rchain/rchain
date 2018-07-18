package coop.rchain.roscala.ob.mbox

import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob.{Ctxt, Nil, Niv, Ob}

class EmptyMbox extends Ob {
  override def receiveMsg(client: MboxOb, task: Ctxt, state: State, globalEnv: GlobalEnv): Ob = {
    MboxOb.logger.debug(s"$this receives message")

    client.mbox = MboxOb.LockedMbox
    client.schedule(task, state, globalEnv)
    Niv
  }

  override def nextMsg(client: MboxOb,
                       newEnabledSet: Ob,
                       state: State,
                       globalEnv: GlobalEnv): Ob = {
    MboxOb.logger.debug(s"Next message received on $this")

    if (newEnabledSet != Nil) {
      val newMbox = QueueMbox(newEnabledSet)
      newMbox.unlock()
      client.mbox = newMbox
    }

    Niv
  }
}
