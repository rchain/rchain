package coop.rchain.rspace
import java.nio.file.{Files, Path}

import coop.rchain.rspace.examples.AddressBookExample._
import coop.rchain.rspace.examples.AddressBookExample.implicits._
import coop.rchain.rspace.util._
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.util.{getK, runK}
import monix.eval.Task
import monix.execution.{CancelableFuture, Scheduler}
import coop.rchain.shared.PathOps._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

object PerformanceDiscoveryMain {
  def main(args: Array[String]): Unit = {
    val dbDir: Path    = Files.createTempDirectory("rchain-storage-test-")
    val mapSize: Long  = 1024L * 1024L * 1024L
    val noTls: Boolean = false

    val context   = Context.create[Channel, Pattern, Entry, EntriesCaptor](dbDir, mapSize, noTls)
    val testStore = LMDBStore.create[Channel, Pattern, Entry, EntriesCaptor](context)
    val space =
      RSpace.create[Channel, Pattern, Entry, Entry, EntriesCaptor](testStore, Branch.MASTER)
    testStore.withTxn(testStore.createTxnWrite())(txn => testStore.clear(txn))

    val tasksCount      = 200//Runtime.getRuntime.availableProcessors
    val iterationsCount = 10

    val channel = Channel("friends#" + 1.toString)

    val channel2 = channel //Channel("friends#" + 2.toString)

    val r3 = space.consume(
      List(channel),
      List(CityMatch(city = "Crystal Lake")),
      new EntriesCaptor,
      persist = true
    )

    println(r3, "?????")

    val s = Scheduler.fixedPool("node-io", 3)

    def createTask(taskIndex: Int, iterations: Int): Task[Long] =
      Task.delay {

        val start   = System.nanoTime()

        for (_ <- 1 to iterations) {
          val r1 = space.produce(channel2, bob, persist = false)
          runK(r1)
          getK(r1).results
        }

        val end  = System.nanoTime()
        val diff = Math.max(end - start, 0)
        diff
      }
    //warm-up pass - short 10 iterations loop should be enough
    val tWarmup = createTask(0, iterationsCount)
    val fWarmup = tWarmup.runAsync(s)
    Await.ready(fWarmup, Duration.Inf)
    println("------------????")
    //benchmark pass
    val tasks = (1 to tasksCount).map(idx => {
      val task = createTask(idx, iterationsCount)
      task
    })

    println("setup")

    val fs: IndexedSeq[Future[Long]] = tasks.map(f => f.executeOn(s).runAsync(s))
//    val t = Task.sequence(tasks)
//    val times = tasks.map(t => Await.result(t, Duration.Inf))
    implicit val xs = s
    val fss = Future.sequence(fs)

    val tx = Await.ready(fss, Duration.Inf)

    println("setup")

    println(s"Finished : $iterationsCount iterations. $tx")
    testStore.close()
    space.close()
    context.close()
    dbDir.recursivelyDelete
  }
}
