package coop.rchain.roscala.pools

import java.util.concurrent.{ForkJoinPool, LinkedBlockingDeque, TimeUnit}

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

  implicit val parallelExecutor = new StrandPoolExecutor[ParallelStrandPool] {
    private val pool     = new ParallelStrandPool
    private val executor = new ForkJoinPool(Runtime.getRuntime.availableProcessors())

    override type Tpe = ParallelStrandPool

    val logger = Logger("StrandPool")

    override def start(vm: Vm): Unit = {
      executor.invoke(vm)
      executor.awaitQuiescence(Long.MaxValue, TimeUnit.MILLISECONDS)
    }

    override def instance = pool
  }

  implicit val simpleExecutor = new StrandPoolExecutor[SimpleStrandPool] {
    private val globalInstance = new SimpleStrandPool

    override type Tpe = SimpleStrandPool

    override def start(vm: Vm): Unit = vm.compute()

    override def instance = globalInstance
  }
}
