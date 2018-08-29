package coop.rchain.roscala.pools

import java.util.concurrent._

import com.typesafe.scalalogging.Logger
import coop.rchain.roscala.Vm

trait StrandPoolExecutor[A] {
  type Tpe <: StrandPool

  def start(vm: Vm): Unit
  def instance: Tpe
}

object StrandPoolExecutor {

  def start[E: StrandPoolExecutor](vm: Vm): Unit =
    implicitly[StrandPoolExecutor[E]].start(vm)

  def instance[E: StrandPoolExecutor](implicit ev: StrandPoolExecutor[E]): ev.Tpe = ev.instance

  implicit def parallelExecutor = new StrandPoolExecutor[ParallelStrandPool] {
    private val tracker  = new Phaser(1)
    private val executor = new ForkJoinPool(Runtime.getRuntime.availableProcessors())
    private val pool     = new ParallelStrandPool(this, tracker)

    override type Tpe = ParallelStrandPool

    val logger = Logger("StrandPool")

    override def start(vm: Vm): Unit = {

      /**
        * Register StrandPoolExecutor to the barrier
        */
      tracker.register()

      executor.invoke(vm)

      /**
        * It will forever wait for completion.
        */
      tracker.arriveAndAwaitAdvance()
    }

    override def instance = pool
  }

  implicit def simpleExecutor = new StrandPoolExecutor[SimpleStrandPool] {
    private val globalInstance = new SimpleStrandPool

    override type Tpe = SimpleStrandPool

    override def start(vm: Vm): Unit = vm.compute()

    override def instance = globalInstance
  }
}
