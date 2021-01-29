package rholang.rosette

import java.nio.file.{Files, Path, Paths}

import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rholang.Resources.mkRuntime
import coop.rchain.rholang.interpreter.{EvaluateResult, InterpreterUtil}
import coop.rchain.shared.{Log, Resources}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FunSuite, Matchers}

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.io.Source

class CompilerTests extends FunSuite with Matchers {
  private val tmpPrefix                   = "rspace-store-"
  private val maxDuration                 = 5.seconds
  implicit val logF: Log[Task]            = new Log.NOPLog[Task]
  implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
  implicit val noopSpan: Span[Task]       = NoopSpan[Task]()

  private val testFiles: Iterator[Path] =
    Files.walk(Paths.get(getClass.getResource("/tests").getPath)).iterator().asScala

  private val failureTestFiles: Iterator[Path] =
    Files.walk(Paths.get(getClass.getResource("/failure_tests").getPath)).iterator().asScala

  for (file <- testFiles if file.getFileName.toString.endsWith(".rho")) {
    test(file.toString.reverse.takeWhile(_ != '/').reverse) {
      val result = execute(file)
      result.errors shouldBe empty
    }
  }

  for (file <- failureTestFiles if file.getFileName.toString.endsWith(".rho")) {
    test(file.toString.reverse.takeWhile(_ != '/').reverse) {
      val result = execute(file)
      result.errors should not be empty
    }
  }

  private def execute(file: Path): EvaluateResult =
    mkRuntime[Task](tmpPrefix)
      .use { runtime =>
        Resources.withResource(Source.fromFile(file.toString))(
          fileContents => {
            implicit val c = runtime.cost
            InterpreterUtil.evaluateResult(runtime, fileContents.mkString)
          }
        )
      }
      .runSyncUnsafe(maxDuration)

}
