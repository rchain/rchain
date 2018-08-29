package coop.rchain.roscala.ob.mbox

import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob.{Ctxt, Nil, Niv, Ob}

class EmptyMbox extends Ob {
  override def receiveMsg(client: MboxOb, task: Ctxt, state: State): Ob = {
    MboxOb.logger.debug(s"$this (mailbox of $client) receives message")
    MboxOb.logger.debug(s"Mailbox of $client gets locked")

    client.mbox = MboxOb.LockedMbox
    client.schedule(task, state)
    Niv
  }

  override def nextMsg(client: MboxOb, newEnabledSet: Ob, state: State): Ob = {
    MboxOb.logger.debug(s"Next message received on $this")

    if (newEnabledSet != Nil) {
      val newMbox = QueueMbox(newEnabledSet)
      newMbox.unlock()
      client.mbox = newMbox
    }

    Niv
  }
}
