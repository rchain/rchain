package coop.rchain.roscala.ob.mbox

import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob.{Ctxt, Niv, Ob}

class QueueMbox(val enabledSet: Ob, val queue: MboxQueue) extends Ob {
  var lockVal: Boolean = true

  override def receiveMsg(client: MboxOb, task: Ctxt, state: State, globalEnv: GlobalEnv): Ob = {
    MboxOb.logger.debug("Queue mailbox receives message")

    if (isLocked || !enabledSet.accepts(task))
      queue.enqueue(task)
    else {
      lock()
      client.schedule(task, state, globalEnv)
    }

    Niv
  }

  def isLocked: Boolean = lockVal

  def lock(): Unit = lockVal = true

  def unlock(): Unit = lockVal = false

  def enqueue(value: Ob): Unit = {
    MboxOb.logger.debug(s"Enqueue $value")
    queue.enqueue(value)
  }
}

object QueueMbox {
  def apply(enabledSet: Ob): QueueMbox = {
    val queue = MboxQueue()
    new QueueMbox(enabledSet, queue)
  }
}
