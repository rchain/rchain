package coop.rchain.rspace.bench

import java.io.{FileNotFoundException, InputStreamReader}
import java.nio.file.{Files, Path}

import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.{Interpreter, Runtime}
import coop.rchain.shared.PathOps.RichPath
import monix.execution.Scheduler.Implicits.global
import org.openjdk.jmh.annotations.{Setup, TearDown}

import scala.concurrent.Await
import scala.concurrent.duration._

trait EvalBenchStateBase {
  private lazy val dbDir: Path = Files.createTempDirectory("rchain-storage-test-")
  private val mapSize: Long    = 1024 * 1024 * 1024

  val rhoScriptSource: String
  lazy val runtime: Runtime  = Runtime.create(dbDir, mapSize)
  val rand: Blake2b512Random = Blake2b512Random(128)
  Await.ready(runtime.reducer.setAvailablePhlos(Cost(Integer.MAX_VALUE)).runAsync, 1.second)
  var term: Option[Par] = None

  @Setup
  def doSetup(): Unit = {
    EvalBenchStateBase.deleteOldStorage(dbDir)

    term = Interpreter.buildNormalizedTerm(resourceFileReader(rhoScriptSource)).runAttempt match {
      case Right(par) => Some(par)
      case Left(err)  => throw err
    }
    //make sure we always start from clean rspace
    runtime.replaySpace.clear()
    runtime.space.clear()
  }

  @TearDown
  def tearDown(): Unit =
    runtime.close()

  def resourceFileReader(path: String): InputStreamReader =
    new InputStreamReader(
      Option(getClass.getResourceAsStream(path))
        .getOrElse(throw new FileNotFoundException(path)))
}

object EvalBenchStateBase {

  /**
    * until we add flag 'delete_lmdb_dir_on_close' for benchmarks and unit-tests
    * this prevents periodic out of disk space failures
    */
  def deleteOldStorage(dbDir: Path): Unit =
    dbDir.getParent.toFile.listFiles
      .filter(dir => dir.isDirectory && (dir.toPath != dbDir))
      .filter(_.getName.startsWith("rchain-storage-test-"))
      .foreach(dir =>
        try {
          println(s"deleting... $dir")
          dir.toPath.recursivelyDelete()
        } catch {
          case _: Exception =>
      })
}
