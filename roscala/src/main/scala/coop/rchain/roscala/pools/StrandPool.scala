package coop.rchain.roscala.pools

import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.ob.Ctxt

trait StrandPool {
  def append(task: (Ctxt, GlobalEnv)): Unit

  def prepend(task: (Ctxt, GlobalEnv)): Unit

  def dequeue: Ctxt

  def isEmpty: Boolean
}
