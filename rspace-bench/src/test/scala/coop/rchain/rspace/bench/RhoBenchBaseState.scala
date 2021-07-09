package coop.rchain.rspace.bench

import coop.rchain.rholang.interpreter.{ParBuilderUtil, ReplayRhoRuntime, RhoRuntime, RholangCLI}
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Par
import coop.rchain.rholang.Resources
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import coop.rchain.shared.Log
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import java.nio.file.{Files, Path}
import scala.concurrent.Await
import scala.concurrent.duration._

abstract class RhoBenchBaseState {

  def setupRho: Option[String] = None
  def testedRho: String

  def execute(bh: Blackhole): Unit = {
    val r = (for {
      result <- runTask
      _      <- runtime.createCheckpoint
    } yield result).unsafeRunSync
    bh.consume(r)
  }

  implicit val scheduler: Scheduler = Scheduler.fixedPool(name = "rho-1", poolSize = 100)
  lazy val dbDir: Path              = Files.createTempDirectory(BenchStorageDirPrefix)

  var runtime: RhoRuntime[Task]             = null
  var replayRuntime: ReplayRhoRuntime[Task] = null
  var setupTerm: Option[Par]                = None
  var term: Par                             = _
  var randSetup: Blake2b512Random           = null
  var randRun: Blake2b512Random             = null

  var runTask: Task[Unit] = null

  implicit val logF: Log[Task]            = Log.log[Task]
  implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
  implicit val noopSpan: Span[Task]       = NoopSpan[Task]()
  implicit val ms: Metrics.Source         = Metrics.BaseSource
  def rand: Blake2b512Random              = Blake2b512Random(128)

  def createRuntime =
    for {
      kvm                         <- RholangCLI.mkRSpaceStoreManager[Task](dbDir)
      store                       <- kvm.rSpaceStores
      spaces                      <- Resources.createRuntimes[Task](store)
      (runtime, replayRuntime, _) = spaces
    } yield (runtime, replayRuntime)

  @Setup(value = Level.Iteration)
  def doSetup(): Unit = {
    deleteOldStorage(dbDir)
    setupTerm = setupRho.flatMap { p =>
      ParBuilderUtil
        .buildNormalizedTerm[Coeval](p)
        .runAttempt match {
        case Right(par) => Some(par)
        case Left(err)  => throw err
      }
    }

    term = ParBuilderUtil
      .buildNormalizedTerm[Coeval](testedRho)
      .runAttempt match {
      case Right(par) => par
      case Left(err)  => throw err
    }

    val runtimes = createRuntime.runSyncUnsafe()
    runtime = runtimes._1
    replayRuntime = runtimes._2
    randSetup = rand
    randRun = rand
    Await
      .result(
        createTest(setupTerm)(runtime, randSetup).runToFuture,
        Duration.Inf
      )
    runTask = createTest(Some(term))(runtime, randRun)
  }

  @TearDown
  def tearDown(): Unit = ()
}
