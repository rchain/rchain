package rholang.rosette

import java.io.FileReader
import java.nio.file.{Files, Path, Paths}

import coop.rchain.rholang.interpreter.{Interpreter, Runtime}
import monix.execution.Scheduler.Implicits.global
import coop.rchain.rholang.Resources.mkRuntime
import monix.eval.Task
import org.scalatest.{FunSuite, Matchers}

import scala.collection.JavaConverters._
import scala.concurrent.duration._

class CompilerTests extends FunSuite with Matchers {
  private val mapSize     = 1024L * 1024L * 10
  private val tmpPrefix   = "rspace-store-"
  private val maxDuration = 5.seconds

  private val testFiles: Iterator[Path] =
    Files.walk(Paths.get(getClass.getResource("/tests").getPath)).iterator().asScala

  private val failureTestFiles: Iterator[Path] =
    Files.walk(Paths.get(getClass.getResource("/failure_tests").getPath)).iterator().asScala

  for (file <- testFiles if file.getFileName.toString.endsWith(".rho")) {
    test(file.toString.reverse.takeWhile(_ != '/').reverse) {
      val result = execute(file)
      assert(result.isRight)
      val resRuntime = result.right.get
      val errorLog   = resRuntime.readAndClearErrorVector()
      assert(errorLog.isEmpty)
    }
  }

  for (file <- failureTestFiles if file.getFileName.toString.endsWith(".rho")) {
    test(file.toString.reverse.takeWhile(_ != '/').reverse) {
      val result = execute(file)
      assert(result.isLeft)
    }
  }

  private def execute(file: Path): Either[Throwable, Runtime] =
    mkRuntime(tmpPrefix, mapSize)
      .use { runtime =>
        Interpreter.execute(runtime, new FileReader(file.toString)).attempt
      }
      .runSyncUnsafe(maxDuration)

}
