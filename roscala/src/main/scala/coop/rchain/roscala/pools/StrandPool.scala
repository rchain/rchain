package coop.rchain.roscala.pools

import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob.Ctxt

trait StrandPool {
  def append(task: (Ctxt, GlobalEnv)): Unit

  def prepend(task: (Ctxt, GlobalEnv)): Unit

  def getNextStrand(state: State): Boolean
}
