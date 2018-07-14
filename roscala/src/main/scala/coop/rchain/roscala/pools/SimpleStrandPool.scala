package coop.rchain.roscala.pools
import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.ob.Ctxt

import scala.collection.mutable

class SimpleStrandPool extends StrandPool {
  private val queue = mutable.Buffer[Ctxt]()

  override def append(task: (Ctxt, GlobalEnv)): Unit = queue.append(task._1)

  override def prepend(task: (Ctxt, GlobalEnv)): Unit = queue.prepend(task._1)

  override def dequeue = queue.remove(queue.size - 1)

  override def isEmpty = queue.isEmpty
}
