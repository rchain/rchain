package coop.rchain.rspace.bench.wide

import coop.rchain.rspace.bench._
import coop.rchain.rholang.interpreter.ChargingReducer
import coop.rchain.rspace.bench.EvalBenchStateBase
import java.io.{FileNotFoundException, InputStreamReader}
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.{Interpreter, Runtime}
import WideBench.{CoarseBenchState, FineBenchState, WideBenchState}
import coop.rchain.shared.StoreType
import monix.eval.Task
import monix.execution.Scheduler
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, _}

class ReplayWideBench {

  @Benchmark
  @BenchmarkMode(Array(Mode.SingleShotTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Fork(value = 1)
  @Threads(1)
  @Warmup(iterations = 2)
  @Measurement(iterations = 5)
  def wideReduce(bh: Blackhole, state: ReplayWideBench.FineBenchState): Unit = {
    implicit val scheduler = state.scheduler
    val result             = state.runReplayTask.unsafeRunSync
    assert(result.isEmpty)
    bh.consume(result)
  }
}

object ReplayWideBench {

  @State(Scope.Benchmark)
  class FineBenchState extends WideBenchState {
    override lazy val runtime: Runtime = Runtime.create(dbDir, mapSize, StoreType.FineGrainedLMDB)
  }

  abstract class WideBenchState {
    val rhoSetupScriptPath: String = "/rholang/wide-setup.rho"
    val rhoScriptSource: String    = "/rholang/wide.rho"

    implicit val scheduler: Scheduler = Scheduler.fixedPool(name = "wide-1", poolSize = 100)
    lazy val dbDir: Path              = Files.createTempDirectory("rchain-storage-test-")
    val mapSize: Long                 = 1024L * 1024L * 1024L * 10L

    lazy val runtime: Runtime           = Runtime.create(dbDir, mapSize)
    implicit val rand: Blake2b512Random = Blake2b512Random(128)
    runtime.reducer.setAvailablePhlos(Cost(Integer.MAX_VALUE)).runSyncUnsafe(1.second)
    runtime.replayReducer.setAvailablePhlos(Cost(Integer.MAX_VALUE)).runSyncUnsafe(1.second)
    var setupTerm: Option[Par] = None
    var term: Option[Par]      = None

    val emptyCheckpoint = runtime.space.createCheckpoint()

    var runReplayTask: Task[Vector[Throwable]] = Task.now(Vector.empty)

    implicit def readErrors = () => runtime.readAndClearErrorVector()

    @Setup(value = Level.Iteration)
    def doSetup(): Unit = {
      deleteOldStorage(dbDir)

      setupTerm =
        Interpreter.buildNormalizedTerm(resourceFileReader(rhoSetupScriptPath)).runAttempt match {
          case Right(par) => Some(par)
          case Left(err)  => throw err
        }

      term = Interpreter.buildNormalizedTerm(resourceFileReader(rhoScriptSource)).runAttempt match {
        case Right(par) => Some(par)
        case Left(err)  => throw err
      }

      //make sure we always start from clean rspace & trie
      runtime.replaySpace.clear()
      runtime.replaySpace.reset(emptyCheckpoint.root)
      runtime.space.clear()
      runtime.space.reset(emptyCheckpoint.root)

      {
        assert(createTest(setupTerm, runtime.reducer).unsafeRunSync.isEmpty)
        val checkpoint = runtime.space.createCheckpoint()
        runtime.replaySpace.rig(checkpoint.root, checkpoint.log)
      }

      assert(createTest(setupTerm, runtime.replayReducer).unsafeRunSync.isEmpty)

      val runTask = createTest(term, runtime.reducer)
      assert(runTask.unsafeRunSync.isEmpty)

      val checkpoint = runtime.space.createCheckpoint()
      runtime.replaySpace.rig(checkpoint.root, checkpoint.log)
      runReplayTask = createTest(term, runtime.replayReducer)
    }

    @TearDown
    def tearDown(): Unit =
      runtime.close()
  }
}
