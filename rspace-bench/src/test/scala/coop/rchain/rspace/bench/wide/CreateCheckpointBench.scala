package coop.rchain.rspace.bench.wide
import java.util.concurrent.TimeUnit

import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.rspace.history.TrieCache
import monix.eval.Task
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

class CreateCheckpointBench {
  @Benchmark
  @BenchmarkMode(Array(Mode.SingleShotTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Fork(value = 1)
  @Threads(1)
  @Warmup(iterations = 1)
  @Measurement(iterations = 1)
  def createCheckpoint_old(bh: Blackhole, state: CreateCheckpointBenchState): Unit = {
    bh.consume(state.runtime.space.createCheckpoint())
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.SingleShotTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Fork(value = 1)
  @Threads(1)
  @Warmup(iterations = 1)
  @Measurement(iterations = 1)
  def createCheckpoint_new(bh: Blackhole, state: CreateCheckpointBenchStateVsCache): Unit = {
    bh.consume(state.runtime.space.createCheckpoint())
  }
}

@State(Scope.Benchmark)
class CreateCheckpointBenchStateVsCache extends CreateCheckpointBenchState {
  override val useCache = true
}

@State(Scope.Benchmark)
class CreateCheckpointBenchState extends WideBenchBaseState {
  override val rhoSetupScriptPath: String = "/rholang/loop-with-wks.rho"
  override val rhoScriptSource: String    = "/rholang/loop-with-wks.rho"

  implicit val rand: Blake2b512Random        = Blake2b512Random(128)
  var runReplayTask: Task[Vector[Throwable]] = Task.now(Vector.empty)

  val useCache = false

  override def doSetup(): Unit = {
    TrieCache.useCache = this.useCache
    super.doSetup()
    val runTask = createTest(term, runtime.reducer)
    assert(runTask.unsafeRunSync.isEmpty)
  }
}