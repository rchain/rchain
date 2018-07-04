package coop.rchain.roscala.ob

import com.typesafe.scalalogging.Logger
import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob.Actor.logger

class Actor extends MboxOb {
  val extension = new Extension()
  meta = Meta.empty

  def addSlot(value: Ob): Int =
    extension.slot += value

  override def lookupAndInvoke(ctxt: Ctxt, state: State): Ob = {
    logger.debug(s"$this receives message")
    // TODO: Add handling of synchronous target
    receive(ctxt, state)
    Suspended
  }

  /**
    * This is the code that is invoked when a message (the task formal)
    * is removed from an actor's mailbox.
    */
  override def schedule(task: Ctxt, state: State): Unit = {
    logger.debug("Schedule task on actor")
    super.lookupAndInvoke(task, state)
  }
}

object Actor {
  val logger = Logger("Actor")
}
