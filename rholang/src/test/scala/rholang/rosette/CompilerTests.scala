package rholang.rosette

import java.io.FileReader
import java.nio.file.{Files, Path, Paths}

import monix.execution.Scheduler.Implicits.global
import coop.rchain.catscontrib.Capture._
import coop.rchain.rholang.interpreter.{Interpreter, Runtime}
import org.scalatest.FunSuite

import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

class CompilerTests extends FunSuite {
  val mapSize     = 1024L * 1024L * 1024L
  val tmpPrefix   = "rspace-store-"
  val maxDuration = 5.seconds

  def runtime = Runtime.create(Files.createTempDirectory(tmpPrefix), mapSize)

  val testFiles: Iterator[Path] =
    Files.walk(Paths.get("tests")).iterator().asScala

  val failureTestFiles: Iterator[Path] =
    Files.walk(Paths.get("failure_tests")).iterator().asScala

  for (file <- testFiles if file.getFileName.toString.endsWith(".rho")) {
    test(file.toString) {
      val result = execute(file)
      assert(result.isSuccess)
    }
  }

  for (file <- failureTestFiles if file.getFileName.toString.endsWith(".rho")) {
    test(file.toString) {
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
