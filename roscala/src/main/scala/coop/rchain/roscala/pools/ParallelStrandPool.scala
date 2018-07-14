package coop.rchain.roscala.pools

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger

import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob.Ctxt
import coop.rchain.roscala.{GlobalEnv, Vm}
import coop.rchain.roscala.pools.StrandPoolExecutor.parallelExecutor

class ParallelStrandPool extends StrandPool {
  private val queue = new LinkedBlockingDeque[Vm]()

  /**
    * This variable is used to indicate if there are some tasks
    * that are currently in progress or ready to be pulled
    */
  private val scheduledCount = new AtomicInteger(0)

  /**
    * Add self to the Executor's queue
    */
  parallelExecutor.addPool(this)

  private def createVm(task: (Ctxt, GlobalEnv)) = {
    val (ctxt, globalEnv) = task

    val newState = State(ctxt = ctxt, globalEnv = globalEnv)(new ParallelStrandPool)
    new Vm(newState.ctxt, newState)
  }

  private def scheduleTask(task: (Ctxt, GlobalEnv), clb: Vm => Unit): Unit = {
    val vm: Vm = createVm(task)

    vm.fork()
    clb(vm)

    scheduledCount.incrementAndGet()
  }

  override def isEmpty: Boolean = scheduledCount.get() == 0

  override def append(task: (Ctxt, GlobalEnv)): Unit =
    scheduleTask(task, queue.addLast)

  override def prepend(task: (Ctxt, GlobalEnv)): Unit =
    scheduleTask(task, queue.addFirst)

  override def dequeue: Ctxt = {
    scheduledCount.decrementAndGet()

    val vm = queue.take()
    vm.join()
    vm.state0.ctxt
  }
}
