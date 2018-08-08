package coop.rchain.roscala.pools

import coop.rchain.roscala.Vm
import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.pools.StrandPoolExecutor.parallelExecutor

class ParallelStrandPool extends StrandPool {
  private def createVm(task: Task) = {
    val (ctxt, state) = task

    val newState = state.copy(exitFlag = false, ctxt = ctxt)(parallelExecutor.instance)
    new Vm(newState.ctxt, newState)
  }

  private def scheduleTask(task: Task): Unit =
    createVm(task).fork()

  override def append(task: Task): Unit =
    scheduleTask(task)

  override def prepend(task: Task): Unit =
    scheduleTask(task)

  override def getNextStrand(state: State) = true
}
