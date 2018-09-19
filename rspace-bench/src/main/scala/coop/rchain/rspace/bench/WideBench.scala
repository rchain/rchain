package coop.rchain.rspace.bench
import java.io.{FileNotFoundException, InputStreamReader}
import java.nio.file.{Files, Path}

import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.{Interpreter, Runtime}
import coop.rchain.rspace.bench.WideBench.WideBenchState
import monix.eval.Task
import monix.execution.Scheduler
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, _}

class WideBench {

  import WideEvalBenchState._

  @Benchmark
  @Threads(1)
  def wideReduce(bh: Blackhole, state: WideBenchState): Unit = {
    implicit val scheduler = state.scheduler
    val runTask            = createTest(state.term, state)
    bh.consume(processErrors(runTask.unsafeRunSync))
  }
}

object WideBench {

  @State(Scope.Benchmark)
  class WideBenchState {
    val rhoSetupScriptPath: String = "/rholang/wide-setup.rho"
    val rhoScriptSource: String    = "/rholang/wide.rho"
    import WideEvalBenchState._

    implicit val scheduler: Scheduler = Scheduler.fixedPool(name = "wide-1", poolSize = 8)
    private lazy val dbDir: Path      = Files.createTempDirectory("rchain-storage-test-")
    private val mapSize: Long         = 1024 * 1024 * 1024

    lazy val runtime: Runtime  = Runtime.create(dbDir, mapSize)
    def rand: Blake2b512Random = Blake2b512Random(128)
    Await.ready(runtime.reducer.setAvailablePhlos(Cost(Integer.MAX_VALUE)).runAsync, 1.second)
    var setupTerm: Option[Par] = None
    var term: Option[Par]      = None

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
      processErrors(Await.result(createTest(setupTerm, this).runAsync, Duration.Inf))
    }

    @TearDown
    def tearDown(): Unit =
      runtime.close()
  }
}

object WideEvalBenchState {
  def processErrors(errors: Vector[Throwable]): Vector[Throwable] = {
    if (errors.nonEmpty) {
      throw new RuntimeException(
        errors
          .map(_.toString())
          .mkString("Errors received during evaluation:\n", "\n", "\n"))
    }
    errors
  }

  def createTest(t: Option[Par], state: WideBenchState): Task[Vector[Throwable]] = {
    val par = t.getOrElse(throw new Error("Failed to prepare executable rholang term"))
    state.runtime.reducer
      .inj(par)(state.rand)
      .map(_ => state.runtime.readAndClearErrorVector())
  }

  def resourceFileReader(path: String): InputStreamReader =
    new InputStreamReader(
      Option(getClass.getResourceAsStream(path))
        .getOrElse(throw new FileNotFoundException(path)))
}
