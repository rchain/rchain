package coop.rchain.rspace.bench

import cats.effect.{Async, IO, Sync}
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rholang.interpreter.RholangCLI
import coop.rchain.rspace.examples.AddressBookExample._
import coop.rchain.rspace.examples.AddressBookExample.implicits._
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import coop.rchain.rspace.{RSpace, _}
import coop.rchain.shared.Log
import coop.rchain.shared.PathOps.RichPath
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import cats.effect.unsafe.implicits.global
class ReplayRSpaceBench {

  import ReplayRSpaceBench._

  @Benchmark
  @BenchmarkMode(Array(Mode.SingleShotTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Warmup(iterations = 1)
  @Fork(value = 1)
  @Measurement(iterations = 1)
  def singleProduce(bh: Blackhole, state: ProduceInMemBenchState) = {
    val res = state.replaySpace.produce(state.produceChannel, bob, persist = true).unsafeRunSync()
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
    val res = state.replaySpace
      .consume(
        List(state.consumeChannel),
        state.matches,
        state.captor,
        persist = true
      )
      .unsafeRunSync()
    assert(res.isDefined)
    bh.consume(res)
  }
}

object ReplayRSpaceBench {

  import scala.concurrent.ExecutionContext.Implicits.global

  abstract class ReplayRSpaceBenchState {

    import cats.effect.unsafe.implicits.global
    var space: ISpace[IO, Channel, Pattern, Entry, EntriesCaptor] = null
    var replaySpace: IReplaySpace[IO, Channel, Pattern, Entry, EntriesCaptor] =
      null
    implicit val logF: Log[IO]            = new Log.NOPLog[IO]
    implicit val noopMetrics: Metrics[IO] = new metrics.Metrics.MetricsNOP[IO]
    implicit val noopSpan: Span[IO]       = NoopSpan[IO]()
    val consumeChannel                    = Channel("consume")
    val produceChannel                    = Channel("produce")
    val matches                           = List(CityMatch(city = "Crystal Lake"))
    val captor                            = new EntriesCaptor()

    def initSpace() = {
      val rigPoint = space.createCheckpoint().unsafeRunSync()
      replaySpace.rigAndReset(rigPoint.root, rigPoint.log).unsafeRunSync()
    }

    private var dbDir: Path = null

    @Setup
    def setup() = {
      dbDir = Files.createTempDirectory("replay-rspace-bench-")
      val kvm   = RholangCLI.mkRSpaceStoreManager[IO](dbDir).unsafeRunSync()
      val store = kvm.rSpaceStores.unsafeRunSync()
      val (space, replaySpace) =
        RSpace.createWithReplay[IO, Channel, Pattern, Entry, EntriesCaptor](store).unsafeRunSync()
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
