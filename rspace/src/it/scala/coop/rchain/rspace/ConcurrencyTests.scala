package coop.rchain.rspace

import cats.Id
import coop.rchain.rspace.examples.AddressBookExample._
import coop.rchain.rspace.examples.AddressBookExample.implicits._
import coop.rchain.rspace.util._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.Await
import scala.collection.concurrent.TrieMap
import scala.concurrent.duration.Duration

trait ConcurrencyTests
    extends StorageTestsBase[Id, Channel, Pattern, Nothing, Entry, EntriesCaptor]
    with TestImplicitHelpers {

  def version: String

  "CORE-589 produce and consume in a multithreaded fashion" should
    "show high performance" in withTestSpace { space =>
    val arrResults      = new Array[Long](Runtime.getRuntime.availableProcessors)
    val iterationsCount = 500

    def createThread(threadIndex: Int, iterations: Int): Thread =
      new Thread(() => {
        val channel = Channel("friends#" + threadIndex.toString)
        val start   = System.nanoTime()

        for (_ <- 1 to iterations) {
          val r1 = space.consume(
            List(channel, channel),
            List(CityMatch(city = "Crystal Lake"), CityMatch(city = "Crystal Lake")),
            new EntriesCaptor,
            persist = false
          )

          r1 shouldBe Right(None)

          val r2 = space.produce(channel, bob, persist = false)

          r2 shouldBe Right(None)

          val r3 = space.produce(channel, bob, persist = false)

          r3 shouldBe defined

          runK(r3)
          getK(r3).results shouldBe List(List(bob, bob))
        }

        val end  = System.nanoTime()
        val diff = Math.max(end - start, 0)

        arrResults(threadIndex) = math.max(arrResults(threadIndex), diff)
      })

    val throwMap = TrieMap[Thread, Throwable]()

    //warm-up pass - short 10 iterations loop should be enough
    val tWarmup = createThread(0, 10)
    tWarmup.setUncaughtExceptionHandler((t, e) => throwMap += t -> e)
    tWarmup.start()
    tWarmup.join()
    arrResults(0) = 0
    //benchmark pass
    val allThreads = arrResults.indices
      .map(index => createThread(index, iterationsCount))
      .map(thread => {
        thread.setUncaughtExceptionHandler((t, e) => throwMap += t -> e)
        thread.start()
        thread
      })
      .toList

    allThreads.foreach(_.join())

    if (throwMap.nonEmpty) {
      throwMap.values.toList match {
        case ex :: tail =>
          val throwEx = new Exception(ex)
          tail.foreach(t => throwEx.addSuppressed(t))
          throw throwEx
        case _ => //impossible
      }
    }

    val min = nsToMs(arrResults.min)
    val max = nsToMs(arrResults.max)
    val avg = arrResults.map(x => nsToMs(x) * (1.0 / arrResults.length)).sum

    println(s"Finished $version: $iterationsCount iterations.")
    println(
      s"Time per thread: avg: ${avg.formatted("%.2f")} ms; min: " +
        s"${min.formatted("%.2f")} ms; max: ${max.formatted("%.2f")} ms")
  }

  "produce and consume with monix.Tasks" should "work" in withTestSpace { space =>
    val tasksCount      = Runtime.getRuntime.availableProcessors
    val iterationsCount = 500

    def createTask(taskIndex: Int, iterations: Int): Task[Long] =
      Task {
        val channel = Channel("friends#" + taskIndex.toString)
        val start   = System.nanoTime()

        for (_ <- 1 to iterations) {
          val r1 = space.produce(channel, bob, persist = false)

          r1 shouldBe Right(None)

          val r2 = space.produce(channel, bob, persist = false)

          r2 shouldBe Right(None)

          val r3 = space
            .consume(
              List(channel, channel),
              List(CityMatch(city = "Crystal Lake"), CityMatch(city = "Crystal Lake")),
              new EntriesCaptor,
              persist = false
            )

          r3 shouldBe defined

          runK(r3)
          getK(r3).results shouldBe List(List(bob, bob))
        }

        val end  = System.nanoTime()
        val diff = Math.max(end - start, 0)
        diff
      }
    //warm-up pass - short 10 iterations loop should be enough
    val tWarmup = createTask(0, iterationsCount)
    val fWarmup = tWarmup.runToFuture
    Await.ready(fWarmup, Duration.Inf)
    //benchmark pass
    val tasks = (1 to tasksCount).map(idx => {
      val task = createTask(idx, iterationsCount)
      task.runToFuture
    })

    val times = tasks.map(t => Await.result(t, Duration.Inf))
    val min   = nsToMs(times.min)
    val max   = nsToMs(times.max)
    val avg   = times.map(x => nsToMs(x) * (1.0 / times.length)).sum

    println(s"Finished $version: $iterationsCount iterations.")
    println(
      s"Time per thread: avg: ${avg.formatted("%.2f")} ms; min: " +
        s"${min.formatted("%.2f")} ms; max: ${max.formatted("%.2f")} ms")
  }

  def nsToMs(nanoseconds: Long): Double =
    nanoseconds * (1.0 / 1000000.0)

  val bob = Entry(name = Name("Bob", "Lahblah"),
                  address = Address("1000 Main St", "Crystal Lake", "Idaho", "223322"),
                  email = "blablah@tenex.net",
                  phone = "698-555-1212")
}

class InMemoryStoreConcurrencyTests
    extends InMemoryStoreStorageExamplesTestsBase[Id]
    with IdTests[Channel, Pattern, Nothing, Entry, EntriesCaptor]
    with ConcurrencyTests {
  override def version: String = "InMemory"
}

class LMDBStoreConcurrencyTestsWithTls
    extends LMDBStoreStorageExamplesTestBase[Id]
      with IdTests[Channel, Pattern, Nothing, Entry, EntriesCaptor]
    with ConcurrencyTests {
  override def version: String = "LMDB with TLS"

  override val noTls: Boolean = false
}

class LMDBStoreConcurrencyTestsNoTls
    extends LMDBStoreStorageExamplesTestBase[Id]
      with IdTests[Channel, Pattern, Nothing, Entry, EntriesCaptor]
    with ConcurrencyTests {

  override def version: String = "LMDB NO_TLS"

  override val noTls: Boolean = true
}
