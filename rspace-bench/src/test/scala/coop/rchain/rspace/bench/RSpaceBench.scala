package coop.rchain.rspace.bench

import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import cats.Id
import cats.effect._
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rspace._
import coop.rchain.rspace.{RSpace, ReplayRSpace}
import coop.rchain.rspace.examples.AddressBookExample._
import coop.rchain.rspace.examples.AddressBookExample.implicits._
import coop.rchain.rspace.util._
import coop.rchain.shared.PathOps._
import coop.rchain.shared.Log
import coop.rchain.store.InMemoryStoreManager
import monix.eval.Task
import monix.execution.Scheduler
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

@org.openjdk.jmh.annotations.State(Scope.Thread)
trait RSpaceBenchBase {

  var space: ISpace[Id, Channel, Pattern, Entry, EntriesCaptor] = null

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
        val r1 = unpackOption(space.produce(channel, bob, persist = false))
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
class RSpaceBench extends RSpaceBenchBase {

  val mapSize: Long  = 1024L * 1024L * 1024L
  val noTls: Boolean = false

  var dbDir: Path                       = null
  implicit val logF: Log[Id]            = new Log.NOPLog[Id]
  implicit val noopMetrics: Metrics[Id] = new metrics.Metrics.MetricsNOP[Id]
  implicit val noopSpan: Span[Id]       = NoopSpan[Id]()
  implicit val kvm                      = InMemoryStoreManager[Id]

  @Setup
  def setup() = {
    dbDir = Files.createTempDirectory("rchain-rspace-bench-")
    space = RSpace.create[Id, Channel, Pattern, Entry, EntriesCaptor](kvm)
  }

  @TearDown
  def tearDown() = {
    space.close()
    dbDir.recursivelyDelete()
    ()
  }
}
