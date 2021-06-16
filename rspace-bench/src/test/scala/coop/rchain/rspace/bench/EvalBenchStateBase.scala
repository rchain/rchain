package coop.rchain.rspace.bench

import cats.effect.Blocker
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.{ParBuilderUtil, RhoRuntime, RholangCLI}
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import coop.rchain.shared.Log
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import org.openjdk.jmh.annotations.{Setup, TearDown}

import java.io.{FileNotFoundException, InputStreamReader}
import java.nio.file.{Files, Path}

trait EvalBenchStateBase {
  val ioScheduler = Scheduler.io()
  val blocker     = Blocker.liftExecutionContext(ioScheduler)

  private lazy val dbDir: Path            = Files.createTempDirectory("rchain-storage-test-")
  implicit val logF: Log[Task]            = new Log.NOPLog[Task]
  implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
  implicit val noopSpan: Span[Task]       = NoopSpan[Task]()
  implicit val kvm                        = RholangCLI.mkRSpaceStoreManager[Task](dbDir, blocker = blocker).runSyncUnsafe()

  val rhoScriptSource: String

  val store                        = kvm.rSpaceStores.unsafeRunSync
  lazy val spaces                  = RhoRuntime.createRuntimes[Task](store).unsafeRunSync
  val (runtime, replayRuntime, hr) = spaces

  val rand: Blake2b512Random = Blake2b512Random(128)
  var term: Option[Par]      = None

  @Setup
  def doSetup(): Unit = {
    deleteOldStorage(dbDir)

    term = ParBuilderUtil
      .buildNormalizedTerm[Coeval](resourceFileReader(rhoScriptSource))
      .runAttempt match {
      case Right(par) => Some(par)
      case Left(err)  => throw err
    }
  }

  @TearDown
  def tearDown(): Unit = ()

  def resourceFileReader(path: String): InputStreamReader =
    new InputStreamReader(
      Option(getClass.getResourceAsStream(path))
        .getOrElse(throw new FileNotFoundException(path))
    )
}
