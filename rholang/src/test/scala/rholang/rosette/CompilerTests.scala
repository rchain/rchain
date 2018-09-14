package rholang.rosette

import java.io.FileReader
import java.nio.file.{Files, Path, Paths}

import coop.rchain.rholang.interpreter.{Interpreter, Runtime}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FunSuite

import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

class CompilerTests extends FunSuite {
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
      assert(result.isSuccess)
    }
  }

  for (file <- failureTestFiles if file.getFileName.toString.endsWith(".rho")) {
    test(file.toString.reverse.takeWhile(_ != '/').reverse) {
      val result = execute(file)
      assert(result.isFailure)
    }
  }

  private def execute(file: Path) = {
    val future = Interpreter.execute(runtime, new FileReader(file.toString)).runAsync

    Try {
      Await.result(future, maxDuration)
    }
  }

}
