package coop.rchain.rholang.interpreter
import java.io.StringReader

import coop.rchain.rholang.Resources.mkRuntime
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class RuntimeSpec extends FlatSpec with Matchers {
  private val mapSize     = 10L * 1024L * 1024L
  private val tmpPrefix   = "rspace-store-"
  private val maxDuration = 5.seconds

//  val runtime = Runtime.create(Files.createTempDirectory(tmpPrefix), mapSize)

  private val channelReadOnlyError = "ReduceError: Trying to read from non-readable channel."

  "rho:io:stdout" should "not get intercepted" in {
    checkError("""new s(`rho:io:stdout`) in { for(x <- s) { Nil } }""", channelReadOnlyError)
  }

  "rho:io:stdoutAck" should "not get intercepted" in {
    checkError("""new s(`rho:io:stdoutAck`) in { for(x <- s) { Nil } }""", channelReadOnlyError)
  }

  "rho:io:stderr" should "not get intercepted" in {
    checkError("""new s(`rho:io:stderr`) in { for(x <- s) { Nil } }""", channelReadOnlyError)
  }

  "rho:io:stderrAck" should "not get intercepted" in {
    checkError("""new s(`rho:io:stderrAck`) in { for(x <- s) { Nil } }""", channelReadOnlyError)
  }

  "rho:registry:lookup" should "not get intercepted" in {
    checkError("""new l(`rho:registry:lookup`) in { for(x <- l) { Nil } }""", channelReadOnlyError)
  }

  "rho:registry:insertArbitrary" should "not get intercepted" in {
    checkError(
      """new i(`rho:registry:insertArbitrary`) in { for(x <- i) { Nil } }""",
      channelReadOnlyError
    )
  }

  private def checkError(rho: String, error: String): Unit =
    failure(rho).getMessage.stripLineEnd should endWith(error)

  private def failure(rho: String): Throwable =
    execute(rho).swap.getOrElse(fail(s"Expected $rho to fail - it didn't."))

  private def execute(source: String): Either[Throwable, Runtime] =
    mkRuntime(tmpPrefix, mapSize)
      .use { runtime =>
        Interpreter
          .execute(runtime, new StringReader(source))
          .attempt
      }
      .runSyncUnsafe(maxDuration)
}
