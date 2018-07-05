package coop.rchain.roscala.ob.mbox

import com.typesafe.scalalogging.Logger
import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob.{Ctxt, Ob}

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
  val logger = Logger("Mailbox")

  val LockedMbox: Ob = new LockedMbox
}
