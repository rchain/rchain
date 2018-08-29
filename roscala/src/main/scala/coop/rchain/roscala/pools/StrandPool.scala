package coop.rchain.roscala.pools

import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob.Ctxt

trait StrandPool {
  type Task = (Ctxt, State)

  def append(task: Task): Unit

  def prepend(task: Task): Unit

  def getNextStrand(state: State): Boolean

  def finish(): Unit
}
