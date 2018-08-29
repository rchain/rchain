package coop.rchain.roscala.ob.mbox

import com.typesafe.scalalogging.Logger
import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob.{Ctxt, Ob}

class MboxOb extends Ob {
  var mbox: Ob = MboxOb.LockedMbox

  /**
    * Only one thread at a time is allowed to call `receive`.
    *
    * Synchronization on `mbox` is necessary since `mbox.receiveMsg`
    * should not be called by thread A while thread B executes
    * `mbox.nextMsg`.
    */
  def receive(ctxt: Ctxt, state: State): Ob = mbox.synchronized {
    ctxt.rcvr = this
    mbox.receiveMsg(this, ctxt, state)
  }

  def schedule(ctxt: Ctxt, state: State): Unit = {
    MboxOb.logger.debug(s"Schedule $ctxt from $this")
    ctxt.scheduleStrand(state)
  }
}

object MboxOb {
  val logger = Logger("Mailbox")

  val EmptyMbox: Ob = new EmptyMbox

  val LockedMbox: Ob = new LockedMbox
}
