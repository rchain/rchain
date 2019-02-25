package coop.rchain.rspace.bench.wide

import coop.rchain.rspace.bench._
import coop.rchain.rholang.interpreter.ChargingReducer
import java.io.{FileNotFoundException, InputStreamReader}
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.{Interpreter, Runtime}
import coop.rchain.shared.{Log, StoreType}
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, _}

abstract class WideBenchBaseState {
  val rhoSetupScriptPath: String = "/rholang/wide-setup.rho"
  val rhoScriptSource: String    = "/rholang/wide.rho"

  implicit val scheduler: Scheduler = Scheduler.fixedPool(name = "wide-1", poolSize = 100)
  lazy val dbDir: Path              = Files.createTempDirectory(BenchStorageDirPrefix)
  val mapSize: Long                 = 1024L * 1024L * 1024L * 10L

  var runtime: Runtime[Task] = null
  var setupTerm: Option[Par] = None
  var term: Option[Par]      = None

  var runTask: Task[Vector[Throwable]] = null

  implicit def readErrors                 = () => runtime.readAndClearErrorVector().unsafeRunSync
  implicit val logF: Log[Task]            = Log.log[Task]
  implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]

  def createRuntime(): Runtime[Task] =
    Runtime.create[Task, Task.Par](dbDir, mapSize, StoreType.LMDB).unsafeRunSync

  @Setup(value = Level.Iteration)
  def doSetup(): Unit = {
    deleteOldStorage(dbDir)
    setupTerm = Interpreter[Coeval]
      .buildNormalizedTerm(resourceFileReader(rhoSetupScriptPath))
      .runAttempt match {
      case Right(par) => Some(par)
      case Left(err)  => throw err
    }

    term = Interpreter[Coeval]
      .buildNormalizedTerm(resourceFileReader(rhoScriptSource))
      .runAttempt match {
      case Right(par) => Some(par)
      case Left(err)  => throw err
    }
    runtime = createRuntime()
    runtime.reducer.setPhlo(Cost(Integer.MAX_VALUE)).runSyncUnsafe(1.second)
    runtime.replayReducer.setPhlo(Cost(Integer.MAX_VALUE)).runSyncUnsafe(1.second)

    (for {
      emptyCheckpoint <- runtime.space.createCheckpoint()
      //make sure we always start from clean rspace & trie
      _ <- runtime.replaySpace.clear()
      _ <- runtime.replaySpace.reset(emptyCheckpoint.root)
      _ <- runtime.space.clear()
    } yield (runtime.space.reset(emptyCheckpoint.root))).unsafeRunSync
  }

  @TearDown
  def tearDown(): Unit =
    runtime.close().unsafeRunSync
}
