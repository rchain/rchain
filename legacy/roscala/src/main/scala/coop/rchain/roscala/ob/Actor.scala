package coop.rchain.roscala.ob

import com.typesafe.scalalogging.Logger
import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob.Actor.logger
import coop.rchain.roscala.ob.mbox.MboxOb

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
    logger.debug(s"Schedule task on $this")
    super.lookupAndInvoke(task, state)
  }

  /** Updates an actor
    *
    * Example:
    * If the actor has a slot `i` (i.e. defined by `(slots& i 0)`)
    * which we want to update to 5, then the `ctxt` would look like
    * this:
    * `ctxt.argvec(0) == Symbol(i)`
    * `ctxt.argvec(1) == Fixnum(5)`
    */
  override def update(enabledSetProvided: Boolean, ctxt: Ctxt, state: State): Ob = {
    logger.debug(s"Update $this")

    val keyStart      = if (enabledSetProvided) 1 else 0
    val newEnabledSet = if (enabledSetProvided) ctxt.argvec(0) else Nil
    var result: Ob    = this

    if (ctxt.nargs > keyStart)
      result = super.update(enabledSetProvided, ctxt, state)

    logger.debug(s"Looking for remaining messages in $mbox")

    /**
      * Only one thread at a time is allowed to call `mbox.nextMsg`.
      *
      * Synchronization on `mbox` is necessary since `mbox.receiveMsg`
      * should not be called by thread A while thread B executes
      * `mbox.nextMsg`.
      */
    mbox.synchronized(
      mbox.nextMsg(this, newEnabledSet, state)
    )

    result
  }
}

object Actor {
  val logger = Logger("Actor")
}
