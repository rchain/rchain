package coop.rchain.rspace.bench
import java.io.{FileNotFoundException, InputStreamReader}
import java.nio.file.{Files, Path}

import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting.{CostAccount, CostAccountingAlg}
import coop.rchain.rholang.interpreter.{Interpreter, Runtime}
import coop.rchain.rspace.bench.WideBench.WideBenchState
import coop.rchain.shared.PathOps.RichPath
import monix.eval.Task
import monix.execution.Scheduler
import org.openjdk.jmh.annotations._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class WideBench {

  import WideEvalBenchState._

  @Benchmark
  @Threads(1)
  def wideReduce(state: WideBenchState): Unit = {
    implicit val scheduler = state.scheduler
    val runTask            = createTest(state.term, state)
    processErrors(Await.result(runTask.runAsync, Duration.Inf))
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

    lazy val runtime: Runtime                   = Runtime.create(dbDir, mapSize)
    val rand: Blake2b512Random                  = Blake2b512Random(128)
    val costAccountAlg: CostAccountingAlg[Task] = CostAccountingAlg.unsafe[Task](CostAccount.zero)
    var setupTerm: Option[Par]                  = None
    var term: Option[Par]                       = None

    /**
      * until we add flag 'delete_lmdb_dir_on_close' for benchmarks and unit-tests
      * this prevents periodic out of disk space failures
      */
    def deleteOldStorage(): Unit =
      dbDir.getParent.toFile.listFiles
        .filter(dir => dir.isDirectory && (dir.toPath != dbDir))
        .filter(_.getName.startsWith("rchain-storage-test-"))
        .foreach(dir =>
          try {
            println(s"deleting... $dir")
            dir.toPath.recursivelyDelete()
          } catch {
            case _: Exception =>
        })

    @Setup
    def doSetup(): Unit = {
      deleteOldStorage()

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
  def processErrors(errors: Vector[Throwable]): Unit = if (errors.nonEmpty) {
    throw new RuntimeException(
      errors
        .map(_.toString())
        .mkString("Errors received during evaluation:\n", "\n", "\n"))
  }

  def createTest(t: Option[Par], state: WideBenchState): Task[Vector[Throwable]] = {
    val par = t.getOrElse(throw new Error("Failed to prepare executable rholang term"))
    state.runtime.reducer
      .inj(par)(state.rand, state.costAccountAlg)
      .map(_ => state.runtime.readAndClearErrorVector())
  }

  def resourceFileReader(path: String): InputStreamReader =
    new InputStreamReader(
      Option(getClass.getResourceAsStream(path))
        .getOrElse(throw new FileNotFoundException(path)))
}
