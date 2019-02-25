package coop.rchain.rspace.bench

import java.io.{FileNotFoundException, InputStreamReader}
import java.nio.file.{Files, Path}

import org.openjdk.jmh.annotations.{Setup, TearDown}
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting.{Cost, CostAccounting}
import coop.rchain.rholang.interpreter.{Interpreter, Runtime}
import coop.rchain.shared.PathOps.RichPath
import coop.rchain.shared.StoreType
import coop.rchain.shared.Log
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler.Implicits.global

trait EvalBenchStateBase {
  private lazy val dbDir: Path            = Files.createTempDirectory("rchain-storage-test-")
  private val mapSize: Long               = 1024L * 1024L * 1024L
  implicit val logF: Log[Task]            = new Log.NOPLog[Task]
  implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]

  val rhoScriptSource: String
  lazy val runtime: Runtime[Task] =
    Runtime.create[Task, Task.Par](dbDir, mapSize, StoreType.LMDB).unsafeRunSync
  val rand: Blake2b512Random = Blake2b512Random(128)
  val costAccountAlg: CostAccounting[Task] =
    CostAccounting.unsafe[Task](Cost(Integer.MAX_VALUE))
  var term: Option[Par] = None

  @Setup
  def doSetup(): Unit = {
    deleteOldStorage(dbDir)

    term = Interpreter[Coeval]
      .buildNormalizedTerm(resourceFileReader(rhoScriptSource))
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
