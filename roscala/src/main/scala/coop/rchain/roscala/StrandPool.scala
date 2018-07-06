package coop.rchain.roscala

import java.util.concurrent._
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.typesafe.scalalogging.Logger
import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob.Ctxt
import coop.rchain.roscala.util.syntax._

object Executor {
  private val numThreads = Runtime.getRuntime.availableProcessors()
  private val executor   = Executors.newFixedThreadPool(numThreads)

  val logger = Logger("StrandPool")

  val pools = new LinkedBlockingDeque[StrandPool]()

  for (n <- 0 to numThreads)
    executor.execute(runner)

  def addPool(strandPool: StrandPool): Unit = pools.addFirst(strandPool)

  def removePool(strandPool: StrandPool): Unit = pools.remove(strandPool)

  private def runner: Runnable = () => {
    while (!executor.isShutdown) {
      val strandPool = pools.pollLast()

      //check for available strandPool
      if (strandPool != null) {

        val task = strandPool.scheduled.poll()
        pools.addFirst(strandPool)

        //check for available task
        if (task != null) {
          val (ctxt, globalEnv) = task
          val state             = State(ctxt = ctxt, globalEnv = globalEnv)

          Vm.run(ctxt, state)

          //Note that state.pc and state.ctxt.pc could be different
          strandPool.completed.put(state.ctxt)
        }
      }
    }
  }
}

class StrandPool {
  val scheduled = new ConcurrentLinkedQueue[(Ctxt, GlobalEnv)]()
  val completed = new LinkedBlockingQueue[Ctxt]()

  /**
    * This variable is used to indicate if there are some tasks
    * that are currently in progress or ready to be pulled
    */
  private val scheduledCount = new AtomicInteger(0)

  /**
    * Add self to the Executor's queue
    */
  Executor.addPool(this)

  def isEmpty = scheduledCount.get() == 0

  def enqueue(task: (Ctxt, GlobalEnv)): Unit = {
    scheduledCount.incrementAndGet()
    scheduled.add(task)
  }

  def dequeue: Ctxt = {
    scheduledCount.decrementAndGet()
    completed.take()
  }
}
