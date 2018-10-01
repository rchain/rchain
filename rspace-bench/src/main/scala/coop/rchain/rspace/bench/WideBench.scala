package coop.rchain.rspace.bench
import coop.rchain.rholang.interpreter.ChargingReducer
import java.io.{FileNotFoundException, InputStreamReader}
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.{Interpreter, Runtime}
import coop.rchain.rspace.bench.WideBench.{CoarseBenchState, FineBenchState, WideBenchState}
import coop.rchain.shared.StoreType
import monix.eval.Task
import monix.execution.Scheduler
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, _}

class WideBench {

  import WideEvalBenchState._

  @Benchmark
  @BenchmarkMode(Array(Mode.SingleShotTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Fork(value = 1)
  @Threads(1)
  @Warmup(iterations = 2)
  @Measurement(iterations = 5)
  def wideReduceCoarse(bh: Blackhole, state: CoarseBenchState): Unit = {
    implicit val scheduler = state.scheduler
    val result             = state.runTask.unsafeRunSync
    bh.consume(processErrors(result))
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.SingleShotTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Fork(value = 1)
  @Threads(1)
  @Warmup(iterations = 2)
  @Measurement(iterations = 5)
  def wideReduceFine(bh: Blackhole, state: FineBenchState): Unit = {
    implicit val scheduler = state.scheduler
    val result             = state.runTask.unsafeRunSync
    bh.consume(processErrors(result))
  }
}

object WideBench {

  @State(Scope.Benchmark)
  class FineBenchState extends WideBenchState {
    override lazy val runtime: Runtime = Runtime.create(dbDir, mapSize, StoreType.FineGrainedLMDB)
  }

  @State(Scope.Benchmark)
  class CoarseBenchState extends WideBenchState {
    override lazy val runtime: Runtime = Runtime.create(dbDir, mapSize)
  }

  abstract class WideBenchState {
    val rhoSetupScriptPath: String = "/rholang/wide-setup.rho"
    val rhoScriptSource: String    = "/rholang/wide.rho"
    import WideEvalBenchState._

    implicit val scheduler: Scheduler = Scheduler.fixedPool(name = "wide-1", poolSize = 100)
    lazy val dbDir: Path              = Files.createTempDirectory("rchain-storage-test-")
    val mapSize: Long                 = 1024L * 1024L * 1024L * 10L

    lazy val runtime: Runtime           = Runtime.create(dbDir, mapSize)
    implicit def rand: Blake2b512Random = Blake2b512Random(128)
    runtime.reducer.setAvailablePhlos(Cost(Integer.MAX_VALUE)).runSyncUnsafe(1.second)
    var setupTerm: Option[Par] = None
    var term: Option[Par]      = None

    var runTask: Task[Vector[Throwable]] = Task.now(Vector.empty)

    implicit def readErrors = () => runtime.readAndClearErrorVector()

    @Setup(value = Level.Iteration)
    def doSetup(): Unit = {
      EvalBenchStateBase.deleteOldStorage(dbDir)

      setupTerm =
        Interpreter.buildNormalizedTerm(resourceFileReader(rhoSetupScriptPath)).runAttempt match {
          case Right(par) => Some(par)
          case Left(err)  => throw err
        }

      term = Interpreter.buildNormalizedTerm(resourceFileReader(rhoScriptSource)).runAttempt match {
        case Right(par) => Some(par)
        case Left(err)  => throw err
      }

      //make sure we always start from clean rspace
      runtime.replaySpace.clear()
      runtime.space.clear()
      processErrors(Await.result(createTest(setupTerm, runtime.reducer).runAsync, Duration.Inf))
      runTask = createTest(term, runtime.reducer)
    }
    @TearDown
    def tearDown(): Unit =
      runtime.close()
  }
}

object WideEvalBenchState {

  def processErrors(errors: Vector[Throwable]): Vector[Throwable] = {

    if (errors.nonEmpty) {
      errors.foreach(_.printStackTrace())
      throw new RuntimeException(
        errors
          .map(_.toString())
          .mkString("Errors received during evaluation:\n", "\n", "\n")
      )
    }
    errors
  }

  def createTest(t: Option[Par], reducer: ChargingReducer[Task])(
      implicit errorProcessor: () => Vector[Throwable],
      rand: Blake2b512Random
  ): Task[Vector[Throwable]] = {
    val par = t.getOrElse(throw new Error("Failed to prepare executable rholang term"))
    reducer
      .inj(par)
      .map(_ => errorProcessor())
  }

  def resourceFileReader(path: String): InputStreamReader =
    new InputStreamReader(
      Option(getClass.getResourceAsStream(path))
        .getOrElse(throw new FileNotFoundException(path))
    )
}
