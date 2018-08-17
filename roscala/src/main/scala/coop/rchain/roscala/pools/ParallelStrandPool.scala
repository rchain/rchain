package coop.rchain.roscala.pools

import java.util.concurrent.Phaser

import coop.rchain.roscala.Vm
import coop.rchain.roscala.Vm.State

class ParallelStrandPool(ex: StrandPoolExecutor[ParallelStrandPool], p: Phaser) extends StrandPool {
  private def createVm(task: Task) = {
    val (ctxt, state) = task

    val newState = state.copy(exitFlag = false, ctxt = ctxt)(ex.instance)
    new Vm(newState.ctxt, newState)
  }

  private def scheduleTask(task: Task): Unit = {

    /**
      * Inform the Phaser that there is task remained
      */
    p.register()
    createVm(task).fork()
  }

  override def append(task: Task): Unit =
    scheduleTask(task)

  override def prepend(task: Task): Unit =
    scheduleTask(task)

  override def getNextStrand(state: State) = true

  /**
    * Inform the Phaser that task has completed
    */
  override def finish: Unit = p.arriveAndDeregister()
}
