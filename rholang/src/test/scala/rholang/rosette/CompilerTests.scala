package rholang.rosette

import java.io.FileReader
import java.nio.file.{Files, Path, Paths}

import coop.rchain.rholang.interpreter.{Interpreter, Runtime}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FunSuite, Matchers}

import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.duration._

class CompilerTests extends FunSuite with Matchers {
  val mapSize     = 1024L * 1024L * 10
  val tmpPrefix   = "rspace-store-"
  val maxDuration = 5.seconds

  def runtime = Runtime.create(Files.createTempDirectory(tmpPrefix), mapSize)

  val testFiles: Iterator[Path] =
    Files.walk(Paths.get(getClass.getResource("/tests").getPath)).iterator().asScala

  val failureTestFiles: Iterator[Path] =
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

  private def execute(file: Path): Either[Throwable, Runtime] = {
    val future = Interpreter.execute(runtime, new FileReader(file.toString)).attempt.runAsync
    Await.result(future, maxDuration)
  }

}
