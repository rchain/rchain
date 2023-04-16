package coop.rchain.rspace.bench

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import coop.rchain.rholang.interpreter.{ReplayRhoRuntime, RhoRuntime, RholangCLI}
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Par
import coop.rchain.rholang.Resources
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import coop.rchain.rholang.interpreter.compiler.Compiler
import coop.rchain.shared.Log
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
    } yield result).unsafeRunSync()
    bh.consume(r)
  }

  implicit val scheduler: Scheduler = Scheduler.fixedPool(name = "rho-1", poolSize = 100)
  lazy val dbDir: Path              = Files.createTempDirectory(BenchStorageDirPrefix)

  var runtime: RhoRuntime[IO]             = null
  var replayRuntime: ReplayRhoRuntime[IO] = null
  var setupTerm: Option[Par]              = None
  var term: Par                           = _
  var randSetup: Blake2b512Random         = null
  var randRun: Blake2b512Random           = null

  var runTask: IO[Unit] = null

  implicit val logF: Log[IO]            = Log.log[IO]
  implicit val noopMetrics: Metrics[IO] = new metrics.Metrics.MetricsNOP[IO]
  implicit val noopSpan: Span[IO]       = NoopSpan[IO]()
  implicit val ms: Metrics.Source       = Metrics.BaseSource
  def rand: Blake2b512Random            = Blake2b512Random.defaultRandom

  def createRuntime =
    for {
      kvm                         <- RholangCLI.mkRSpaceStoreManager[IO](dbDir)
      store                       <- kvm.rSpaceStores
      spaces                      <- Resources.createRuntimes[IO](store)
      (runtime, replayRuntime, _) = spaces
    } yield (runtime, replayRuntime)

  @Setup(value = Level.Iteration)
  def doSetup(): Unit = {
    deleteOldStorage(dbDir)
    setupTerm = setupRho.flatMap { p =>
      Compiler[IO].sourceToADT(p).attempt.unsafeRunSync() match {
        case Right(par) => Some(par)
        case Left(err)  => throw err
      }
    }

    term = Compiler[IO].sourceToADT(testedRho).attempt.unsafeRunSync() match {
      case Right(par) => par
      case Left(err)  => throw err
    }

    val runtimes = createRuntime.unsafeRunSync()
    runtime = runtimes._1
    replayRuntime = runtimes._2
    randSetup = rand
    randRun = rand
    Await
      .result(
        createTest(setupTerm)(runtime, randSetup).unsafeToFuture(),
        Duration.Inf
      )
    runTask = createTest(Some(term))(runtime, randRun)
  }

  @TearDown
  def tearDown(): Unit = ()
}
