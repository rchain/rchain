package coop.rchain.rspace.bench

import java.io.{FileNotFoundException, InputStreamReader}
import java.nio.file.{Files, Path}

import org.openjdk.jmh.annotations.{Setup, TearDown}
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting.{Cost, CostAccount, CostAccounting}
import coop.rchain.rholang.interpreter.{ChargingReducer, Interpreter, Runtime}
import coop.rchain.shared.PathOps.RichPath
import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.duration._

trait EvalBenchStateBase {
  private lazy val dbDir: Path = Files.createTempDirectory("rchain-storage-test-")
  private val mapSize: Long    = 1024L * 1024L * 1024L

  val rhoScriptSource: String
  lazy val runtime: Runtime           = Runtime.create(dbDir, mapSize)
  implicit val rand: Blake2b512Random = Blake2b512Random(128)
  val costAccountAlg: CostAccounting[Task] =
    CostAccounting.unsafe[Task](CostAccount(Integer.MAX_VALUE))
  var term: Option[Par] = None

  implicit def readErrors: () => Vector[Throwable] = () => runtime.readAndClearErrorVector()

  @Setup
  def doSetup(): Unit = {
    deleteOldStorage(dbDir)

    implicit val scheduler: Scheduler = monix.execution.Scheduler.Implicits.global

    runtime.reducer.setAvailablePhlos(Cost(Integer.MAX_VALUE)).unsafeRunSync
    runtime.replayReducer.setAvailablePhlos(Cost(Integer.MAX_VALUE)).unsafeRunSync

    term = Interpreter.buildNormalizedTerm(resourceFileReader(rhoScriptSource)).runAttempt match {
      case Right(par) => Some(par)
      case Left(err)  => throw err
    }
    val emptyCheckpoint = runtime.space.createCheckpoint()
    //make sure we always start from clean rspace & trie
    runtime.replaySpace.clear()
    runtime.replaySpace.reset(emptyCheckpoint.root)
    runtime.space.clear()
    runtime.space.reset(emptyCheckpoint.root)
  }

  @TearDown
  def tearDown(): Unit =
    runtime.close()

  def resourceFileReader(path: String): InputStreamReader =
    new InputStreamReader(
      Option(getClass.getResourceAsStream(path))
        .getOrElse(throw new FileNotFoundException(path))
    )

  def createTest(t: Option[Par], reducer: ChargingReducer[Task])(
      implicit errorProcessor: () => Vector[Throwable],
      rand: Blake2b512Random
  ): Task[Vector[Throwable]] = {
    val par = t.getOrElse(throw new Error("Failed to prepare executable rholang term"))
    reducer
      .inj(par)
      .map(_ => errorProcessor())
  }
}
