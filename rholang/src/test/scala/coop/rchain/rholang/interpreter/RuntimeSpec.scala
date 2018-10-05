package coop.rchain.rholang.interpreter
import java.io.StringReader
import java.nio.file.Files

import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class RuntimeSpec extends FlatSpec with Matchers {
  val mapSize     = 10L * 1024L * 1024L
  val tmpPrefix   = "rspace-store-"
  val maxDuration = 5.seconds

  val runtime = Runtime.create(Files.createTempDirectory(tmpPrefix), mapSize)

  "rho:io:stdout" should "not get intercepted" in {
    val intercept =
      """new stdout(`rho:io:stdout`) in { stdout!("foo", "bar") | for (...@x <- stdout) { stdout!(x) } }"""

    failure(intercept).getMessage.stripLineEnd should endWith(
      "ReduceError: Trying to read from non-readable channel."
    )
  }

  "rho:io:stdoutAck" should "not get intercepted" in {
    val intercept =
      """new ackCh, stdoutAck(`rho:io:stdoutAck`) in { stdoutAck!("foo") | for (@x <- stdoutAck) { stdoutAck!(x, *ackCh) } }"""

    failure(intercept).getMessage.stripLineEnd should endWith(
      "ReduceError: Trying to read from non-readable channel."
    )
  }

  "rho:io:stderr" should "not get intercepted" in {
    val intercept =
      """new stderr(`rho:io:stderr`) in { stderr!("foo", "bar") | for (...@x <- stderr) { stderr!(x) } }"""

    failure(intercept).getMessage.stripLineEnd should endWith(
      "ReduceError: Trying to read from non-readable channel."
    )
  }

  "rho:io:stderrAck" should "not get intercepted" in {
    val intercept =
      """new ackCh, stderrAck(`rho:io:stderrAck`) in { stderrAck!("foo") | for (@x <- stderrAck) { stderrAck!(x, *ackCh) } }"""

    failure(intercept).getMessage.stripLineEnd should endWith(
      "ReduceError: Trying to read from non-readable channel."
    )
  }

  private def failure(rho: String): Throwable =
    execute(rho).swap.getOrElse(fail(s"Expected $rho to fail - it didn't."))

  private def execute(source: String): Either[Throwable, Runtime] =
    Interpreter
      .execute(runtime, new StringReader(source))
      .attempt
      .runSyncUnsafe(maxDuration)
}
