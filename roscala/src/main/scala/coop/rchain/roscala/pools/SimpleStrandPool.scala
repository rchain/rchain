package coop.rchain.roscala.pools
import com.typesafe.scalalogging.Logger
import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob.Ctxt

import scala.collection.mutable

class SimpleStrandPool extends StrandPool {
  private val queue  = mutable.Buffer[Ctxt]()
  private val logger = Logger("SimpleStrandPool")

  override def append(task: (Ctxt, GlobalEnv)): Unit = queue.append(task._1)

  override def prepend(task: (Ctxt, GlobalEnv)): Unit = queue.prepend(task._1)

  override def getNextStrand(state: State): Boolean =
    if (queue.isEmpty) {
      logger.debug("Empty strandPool")
      true
    } else {
      val ctxt = queue.remove(queue.size - 1)

      logger.debug(s"Ctxt completed. Install $ctxt")

      // Install `ctxt`
      state.ctxt = ctxt
      state.code = ctxt.code
      state.pc = ctxt.pc

      false
    }

}
