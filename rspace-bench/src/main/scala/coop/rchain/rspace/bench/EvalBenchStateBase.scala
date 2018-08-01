package coop.rchain.rspace.bench

import java.io.{FileNotFoundException, InputStreamReader}
import java.nio.file.{Files, Path}

import org.openjdk.jmh.annotations.{Setup, TearDown}
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting.{CostAccount, CostAccountingAlg}
import coop.rchain.rholang.interpreter.{Interpreter, Runtime}
import coop.rchain.shared.PathOps.RichPath
import monix.eval.Task

trait EvalBenchStateBase {
  private val dbDir: Path   = Files.createTempDirectory("rchain-storage-test-")
  private val mapSize: Long = 1024 * 1024 * 1024

  val rhoScriptSource: String
  val runtime: Runtime                        = Runtime.create(dbDir, mapSize)
  val rand: Blake2b512Random                  = Blake2b512Random(128)
  val costAccountAlg: CostAccountingAlg[Task] = CostAccountingAlg.unsafe[Task](CostAccount.zero)
  var term: Option[Par]                       = None

  /**
    * until we add flag 'delete_lmdb_dir_on_close' for benchmarks and unit-tests
    * this prevents periodic out of disk space failures
    */
  def deleteOldStorages(): Unit =
    dbDir.getParent.toFile.listFiles
      .filter(dir => dir.isDirectory && (dir != dbDir))
      .filter(_.getName.startsWith("rchain-storage-test-"))
      .foreach(dir =>
        try {
          println(s"deleting... $dir")
          dir.toPath.recursivelyDelete()
        } catch {
          case _: Exception =>
      })

  @Setup
  def doSetup(): Unit = {
    deleteOldStorages()

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
