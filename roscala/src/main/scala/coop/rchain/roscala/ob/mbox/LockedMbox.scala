package coop.rchain.roscala.ob.mbox

import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob.{Ctxt, Nil, Niv, Ob}

class LockedMbox extends Ob {
  override def receiveMsg(client: MboxOb, task: Ctxt, state: State): Ob = {
    MboxOb.logger.debug(s"$this receives message")

    val newMbox = QueueMbox(Nil)
    newMbox.enqueue(task)
    client.mbox = newMbox
    Niv
  }

  /**
    * `nextMsg` is called after an `Actor` finished updating its
    * state. It gets called when an `Actor` is ready to process more
    * messages.
    *
    * In the case of a `LockedMbox` the only message in the mailbox
    * already got processed.
    * Therefore all that needs to be done is to unlock the given
    * mailbox.
    */
  override def nextMsg(client: MboxOb, newEnabledSet: Ob, state: State): Ob = {
    MboxOb.logger.debug(s"Unlock ${client}'s $this")

    if (newEnabledSet == Nil) {
      MboxOb.logger.debug(s"No more messages to process - become EmptyMbox")
      client.mbox = MboxOb.EmptyMbox
    } else {
      MboxOb.logger.debug(s"There are messages left to process - become unlocked QueueMbox")
      val newMbox = QueueMbox(newEnabledSet)
      newMbox.unlock()
      client.mbox = newMbox
    }

    Niv
  }
}
