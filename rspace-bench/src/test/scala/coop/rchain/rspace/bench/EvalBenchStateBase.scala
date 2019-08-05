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
import coop.rchain.metrics.Span.TraceId
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.{ParBuilderUtil, Runtime}
import coop.rchain.shared.Log
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler.Implicits.global
import org.openjdk.jmh.annotations.{Setup, TearDown}

trait EvalBenchStateBase {
  private lazy val dbDir: Path            = Files.createTempDirectory("rchain-storage-test-")
  private val mapSize: Long               = 1024L * 1024L * 1024L
  implicit val logF: Log[Task]            = new Log.NOPLog[Task]
  implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
  implicit val noopSpan: Span[Task]       = NoopSpan[Task]()
  implicit val traceId: TraceId           = Span.empty

  val rhoScriptSource: String
  lazy val runtime: Runtime[Task] =
    Runtime.createWithEmptyCost[Task](dbDir, mapSize).unsafeRunSync
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
    //make sure we always start from clean rspace
    runtime.replaySpace.clear()
    runtime.space.clear()
  }

  @TearDown
  def tearDown(): Unit =
    runtime.close().unsafeRunSync

  def resourceFileReader(path: String): InputStreamReader =
    new InputStreamReader(
      Option(getClass.getResourceAsStream(path))
        .getOrElse(throw new FileNotFoundException(path))
    )
}
