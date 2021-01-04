package coop.rchain.rspace.bench

import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import cats.Id
import cats.effect._
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rspace._
import coop.rchain.rspace.{RSpace, ReplayRSpace}
import coop.rchain.rspace.ISpace.IdISpace
import coop.rchain.rspace.examples.AddressBookExample._
import coop.rchain.rspace.examples.AddressBookExample.implicits._
import coop.rchain.shared.Log
import coop.rchain.shared.PathOps.RichPath
import coop.rchain.store.InMemoryStoreManager
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

class ReplayRSpaceBench {

  import ReplayRSpaceBench._

  @Benchmark
  @BenchmarkMode(Array(Mode.SingleShotTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Warmup(iterations = 1)
  @Fork(value = 1)
  @Measurement(iterations = 1)
  def singleProduce(bh: Blackhole, state: ProduceInMemBenchState) = {
    val res = state.replaySpace.produce(state.produceChannel, bob, persist = true)
    assert(res.isDefined)
    bh.consume(res)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.SingleShotTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Warmup(iterations = 1)
  @Fork(value = 1)
  @Measurement(iterations = 1)
  def singleConsume(bh: Blackhole, state: ConsumeInMemBenchState) = {
    val res = state.replaySpace.consume(
      List(state.consumeChannel),
      state.matches,
      state.captor,
      persist = true
    )
    assert(res.isDefined)
    bh.consume(res)
  }
}

object ReplayRSpaceBench {

  import scala.concurrent.ExecutionContext.Implicits.global

  abstract class ReplayRSpaceBenchState {
    var space: IdISpace[Channel, Pattern, Entry, EntriesCaptor] = null
    var replaySpace: IReplaySpace[cats.Id, Channel, Pattern, Entry, EntriesCaptor] =
      null
    implicit val logF: Log[Id]            = new Log.NOPLog[Id]
    implicit val noopMetrics: Metrics[Id] = new metrics.Metrics.MetricsNOP[Id]
    implicit val noopSpan: Span[Id]       = NoopSpan[Id]()
    val consumeChannel                    = Channel("consume")
    val produceChannel                    = Channel("produce")
    val matches                           = List(CityMatch(city = "Crystal Lake"))
    val captor                            = new EntriesCaptor()
    implicit val kvm                      = InMemoryStoreManager[Id]

    def initSpace() = {
      val rigPoint = space.createCheckpoint()
      replaySpace.rigAndReset(rigPoint.root, rigPoint.log)
    }

    private var dbDir: Path = null

    @Setup
    def setup() = {
      dbDir = Files.createTempDirectory("replay-rspace-bench-")
      val (space, replaySpace, _) =
        RSpace.createWithReplay[Id, Channel, Pattern, Entry, EntriesCaptor](kvm)
      this.space = space
      this.replaySpace = replaySpace
    }

    @TearDown
    def tearDown() = {
      dbDir.recursivelyDelete()
      ()
    }
  }

  @State(Scope.Thread)
  class ConsumeInMemBenchState extends ReplayRSpaceBenchState {

    def prepareConsume() = {
      (1 to 1000).foreach { _ =>
        space.produce(consumeChannel, bob, persist = true)

      }
      (1 to 2).foreach { i =>
        space.consume(
          List(consumeChannel),
          matches,
          captor,
          persist = true
        )
      }
    }
    @Setup
    override def setup() = {
      super.setup()
      prepareConsume()
      initSpace
    }
  }

  @State(Scope.Thread)
  class ProduceInMemBenchState extends ReplayRSpaceBenchState {

    def prepareProduce() = {
      (1 to 1000).foreach { _ =>
        space.consume(
          List(produceChannel),
          matches,
          captor,
          persist = true
        )
      }
      (1 to 2).foreach { i =>
        space.produce(produceChannel, bob, persist = true)
      }
    }
    @Setup
    override def setup() = {
      super.setup()
      prepareProduce()
      initSpace
    }
  }
}
