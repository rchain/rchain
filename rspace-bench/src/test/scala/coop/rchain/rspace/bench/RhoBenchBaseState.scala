package coop.rchain.rspace.bench

import cats.Eval
import cats.implicits.catsSyntaxOptionId
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Par
import coop.rchain.rholang.Resources
import coop.rchain.rholang.interpreter.compiler.Compiler
import coop.rchain.rholang.interpreter.{ReplayRhoRuntime, RhoRuntime, RholangCLI}
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import coop.rchain.catscontrib.effect.implicits.sEval

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
    } yield result).runSyncUnsafe()
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
  def rand: Blake2b512Random              = Blake2b512Random.defaultRandom

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
      try {
        Compiler[Eval].sourceToADT(p).value.some
      } catch { case err: Throwable => throw err }
    }

    term = try {
      Compiler[Eval].sourceToADT(testedRho).value
    } catch { case err: Throwable => throw err }

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
