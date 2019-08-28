package coop.rchain.rspace.bench.wide

import coop.rchain.rspace.bench._
import coop.rchain.rholang.interpreter.{ParBuilderUtil, Runtime}
import java.io.{FileNotFoundException, InputStreamReader}
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.shared.Log
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
  implicit val noopSpan: Span[Task]       = NoopSpan[Task]()
  implicit val ms: Metrics.Source         = Metrics.BaseSource

  def createRuntime(): Runtime[Task] =
    (for {
      cost <- CostAccounting.emptyCost[Task]
      runtime <- {
        implicit val c: _cost[Task] = cost
        Runtime.create[Task, Task.Par](dbDir, mapSize)
      }
    } yield (runtime)).unsafeRunSync

  @Setup(value = Level.Iteration)
  def doSetup(): Unit = {
    deleteOldStorage(dbDir)
    setupTerm = ParBuilderUtil
      .buildNormalizedTerm[Coeval](resourceFileReader(rhoSetupScriptPath))
      .runAttempt match {
      case Right(par) => Some(par)
      case Left(err)  => throw err
    }

    term = ParBuilderUtil
      .buildNormalizedTerm[Coeval](resourceFileReader(rhoScriptSource))
      .runAttempt match {
      case Right(par) => Some(par)
      case Left(err)  => throw err
    }
    runtime = createRuntime()
    runtime.cost.set(Cost.UNSAFE_MAX).runSyncUnsafe(1.second)

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
