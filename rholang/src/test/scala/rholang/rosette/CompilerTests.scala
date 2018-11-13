package rholang.rosette

import java.io.FileReader
import java.nio.file.{Files, Path, Paths}

import cats.Parallel
import coop.rchain.rholang.interpreter.{Interpreter, Runtime}
import coop.rchain.rholang.Resources.mkRuntime
import org.scalatest.{FunSuite, Matchers}

import scala.collection.JavaConverters._
import coop.rchain.catscontrib.effect.implicits._

import scala.util.Try

class CompilerTests extends FunSuite with Matchers {
  private val mapSize   = 1024L * 1024L * 10
  private val tmpPrefix = "rspace-store-"

  private val testFiles: Iterator[Path] =
    Files.walk(Paths.get(getClass.getResource("/tests").getPath)).iterator().asScala

  private val failureTestFiles: Iterator[Path] =
    Files.walk(Paths.get(getClass.getResource("/failure_tests").getPath)).iterator().asScala

  for (file <- testFiles if file.getFileName.toString.endsWith(".rho")) {
    test(file.toString.reverse.takeWhile(_ != '/').reverse) {
      val result = execute(file)
      assert(result.isSuccess)
      val resRuntime = result.get
      val errorLog   = resRuntime.readAndClearErrorVector().get
      assert(errorLog.isEmpty)
    }
  }

  for (file <- failureTestFiles if file.getFileName.toString.endsWith(".rho")) {
    test(file.toString.reverse.takeWhile(_ != '/').reverse) {
      val result = execute(file)
      assert(result.isFailure)
    }
  }

  implicit val parallelTry: Parallel[Try, Try] = Parallel.identity[Try]

  private def execute(file: Path): Try[Runtime[Try]] =
    mkRuntime[Try, Try](tmpPrefix, mapSize)
      .use { runtime =>
        Interpreter.execute[Try](runtime, new FileReader(file.toString))
      }

}
