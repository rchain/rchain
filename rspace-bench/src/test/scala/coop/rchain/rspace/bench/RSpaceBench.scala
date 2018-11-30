package coop.rchain.rspace.bench

import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import cats.Id
import cats.effect._
import coop.rchain.rspace._
import coop.rchain.rspace.examples.AddressBookExample._
import coop.rchain.rspace.examples.AddressBookExample.implicits._
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.util._
import coop.rchain.shared.PathOps._
import monix.eval.Task
import monix.execution.Scheduler
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

import scala.concurrent.ExecutionContext.Implicits.global

@org.openjdk.jmh.annotations.State(Scope.Thread)
trait RSpaceBench {

  var space: ISpace[Id, Channel, Pattern, Nothing, Entry, Entry, EntriesCaptor] = null

  val channel  = Channel("friends#" + 1.toString)
  val channels = List(channel)
  val matches  = List(CityMatch(city = "Crystal Lake"))
  val captor   = new EntriesCaptor()

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def measureAvgConsumeTime(bh: Blackhole) = {
    val r = space.consume(
      channels,
      matches,
      captor,
      persist = true
    )
    bh.consume(r)
  }

  def createTask(taskIndex: Int, iterations: Int): Task[Unit] =
    Task.delay {
      for (_ <- 1 to iterations) {
        val r1 = space.produce(channel, bob, persist = false)
        runK(r1)
        getK(r1).results
      }
    }

  val tasksCount      = 200
  val iterationsCount = 10
  val tasks = (1 to tasksCount).map(idx => {
    val task = createTask(idx, iterationsCount)
    task
  })

  val dupePool = Scheduler.fixedPool("dupe-pool", 3)

  @Benchmark
  @BenchmarkMode(Array(Mode.SingleShotTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Warmup(iterations = 0)
  @Threads(1)
  def simulateDupe(bh: Blackhole) = {

    space.consume(
      channels,
      matches,
      captor,
      persist = true
    )

    val results: IndexedSeq[Future[Unit]] =
      tasks.map(f => f.executeOn(dupePool).runToFuture(dupePool))

    bh.consume(Await.ready(Future.sequence(results), Duration.Inf))
  }
}

@org.openjdk.jmh.annotations.State(Scope.Thread)
@Warmup(iterations = 1)
@Fork(value = 2)
@Measurement(iterations = 10)
class LMDBBench extends RSpaceBench {

  val mapSize: Long  = 1024L * 1024L * 1024L
  val noTls: Boolean = false

  var dbDir: Path = null

  @Setup
  def setup() = {
    dbDir = Files.createTempDirectory("rchain-rspace-lmdb-bench-")
    val context   = Context.create[Channel, Pattern, Entry, EntriesCaptor](dbDir, mapSize, noTls)
    val testStore = LMDBStore.create[Channel, Pattern, Entry, EntriesCaptor](context)
    assert(testStore.toMap.isEmpty)
    space = RSpace.create[Id, Channel, Pattern, Nothing, Entry, Entry, EntriesCaptor](
      testStore,
      Branch.MASTER
    )
  }

  @TearDown
  def tearDown() = {
    space.close()
    dbDir.recursivelyDelete()
    ()
  }
}

@org.openjdk.jmh.annotations.State(Scope.Thread)
@Warmup(iterations = 1)
@Fork(value = 2)
@Measurement(iterations = 10)
class InMemBench extends RSpaceBench {

  @Setup
  def setup() = {
    val context = Context.createInMemory[Channel, Pattern, Entry, EntriesCaptor]()
    assert(context.trieStore.toMap.isEmpty)
    val testStore = InMemoryStore.create(context.trieStore, Branch.MASTER)
    assert(testStore.toMap.isEmpty)
    space = RSpace.create[Id, Channel, Pattern, Nothing, Entry, Entry, EntriesCaptor](
      testStore,
      Branch.MASTER
    )
  }

  @TearDown
  def tearDown() = {
    space.close()
    ()
  }
}

@org.openjdk.jmh.annotations.State(Scope.Thread)
@Warmup(iterations = 1)
@Fork(value = 2)
@Measurement(iterations = 10)
class MixedBench extends RSpaceBench {

  val mapSize: Long  = 1024L * 1024L * 1024L
  val noTls: Boolean = false

  var dbDir: Path = null

  @Setup
  def setup() = {
    dbDir = Files.createTempDirectory("rchain-rspace-mixed-bench-")
    val context = Context.createMixed[Channel, Pattern, Entry, EntriesCaptor](dbDir, mapSize)
    assert(context.trieStore.toMap.isEmpty)
    val testStore = InMemoryStore.create(context.trieStore, Branch.MASTER)
    assert(testStore.toMap.isEmpty)
    space = RSpace.create[Id, Channel, Pattern, Nothing, Entry, Entry, EntriesCaptor](
      testStore,
      Branch.MASTER
    )
  }

  @TearDown
  def tearDown() = {
    space.close()
    dbDir.recursivelyDelete()
    ()
  }
}
