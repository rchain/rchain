package coop.rchain.roscala.pools

import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob.Ctxt
import coop.rchain.roscala.pools.StrandPoolExecutor.parallelExecutor
import coop.rchain.roscala.{GlobalEnv, Vm}

class ParallelStrandPool extends StrandPool {

  /**
    * Add self to the Executor's queue
    */
  parallelExecutor.addPool(this)

  private def createVm(task: (Ctxt, GlobalEnv)) = {
    val (ctxt, globalEnv) = task

    val newState = State(ctxt = ctxt, globalEnv = globalEnv)(new ParallelStrandPool)
    new Vm(newState.ctxt, newState)
  }

  private def scheduleTask(task: (Ctxt, GlobalEnv)): Unit =
    createVm(task).fork()

  override def append(task: (Ctxt, GlobalEnv)): Unit =
    scheduleTask(task)

  override def prepend(task: (Ctxt, GlobalEnv)): Unit =
    scheduleTask(task)

  override def getNextStrand(state: State) = true
}
