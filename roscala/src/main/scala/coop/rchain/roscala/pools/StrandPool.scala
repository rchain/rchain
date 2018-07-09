package coop.rchain.roscala.pools

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger

import com.typesafe.scalalogging.Logger
import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob.Ctxt
import coop.rchain.roscala.{GlobalEnv, Vm}

object Executor {
  private[pools] val executor = new ForkJoinPool(Runtime.getRuntime.availableProcessors())
  private[pools] val pools    = new LinkedBlockingDeque[StrandPool]()

  val logger = Logger("StrandPool")

  def start(vm: Vm): Unit =
    executor.invoke(vm)
}

class StrandPool {
  private val queue = new LinkedBlockingDeque[Vm]()

  /**
    * This variable is used to indicate if there are some tasks
    * that are currently in progress or ready to be pulled
    */
  private val scheduledCount = new AtomicInteger(0)

  /**
    * Add self to the Executor's queue
    */
  Executor.pools.addFirst(this)

  private def createVm(task: (Ctxt, GlobalEnv)) = {
    val (ctxt, globalEnv) = task

    val newState = State(ctxt = ctxt, globalEnv = globalEnv)
    new Vm(newState.ctxt, newState)
  }

  def isEmpty: Boolean = scheduledCount.get() == 0

  def enqueue(task: (Ctxt, GlobalEnv)): Unit =
    scheduleTask(task, queue.addLast)

  def push(task: (Ctxt, GlobalEnv)): Unit =
    scheduleTask(task, queue.addFirst)

  def scheduleTask(task: (Ctxt, GlobalEnv), clb: Vm => Unit): Unit = {
    val vm: Vm = createVm(task)

    vm.fork()
    clb(vm)

    scheduledCount.incrementAndGet()
  }

  def dequeue: Ctxt = {
    scheduledCount.decrementAndGet()

    val vm = queue.take()
    vm.join()
    vm.state0.ctxt
  }
}
