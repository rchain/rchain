package coop.rchain.rspace.bench

import cats._
import cats.data._
import cats.implicits._
import cats.effect._
import java.io.{FileNotFoundException, InputStreamReader}
import java.nio.file.{Files, Path}

import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.{ParBuilderUtil, RhoRuntime}
import coop.rchain.shared.Log
import coop.rchain.store.InMemoryStoreManager
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler.Implicits.global
import org.openjdk.jmh.annotations.{Setup, TearDown}

trait EvalBenchStateBase {
  private lazy val dbDir: Path            = Files.createTempDirectory("rchain-storage-test-")
  private val mapSize: Long               = 1024L * 1024L * 1024L
  implicit val logF: Log[Task]            = new Log.NOPLog[Task]
  implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
  implicit val noopSpan: Span[Task]       = NoopSpan[Task]()
  implicit val kvm                        = InMemoryStoreManager[Task]

  val rhoScriptSource: String

  lazy val space = RhoRuntime.setupRhoRSpace[Task](dbDir, mapSize, kvm).unsafeRunSync
  lazy val runtime: RhoRuntime[Task] =
    RhoRuntime.createRhoRuntime[Task](space).unsafeRunSync
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
