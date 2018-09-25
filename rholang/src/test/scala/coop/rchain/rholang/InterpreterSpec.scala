package coop.rchain.rholang

import java.io.StringReader
import java.nio.file.Files

import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.interpreter.{Interpreter, Runtime}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class InterpreterSpec extends FlatSpec with Matchers {
  val mapSize     = 10L * 1024L * 1024L
  val tmpPrefix   = "rspace-store-"
  val maxDuration = 5.seconds

  val runtime = Runtime.create(Files.createTempDirectory(tmpPrefix), mapSize)

  "Interpreter" should "restore RSpace to its prior state after evaluation error" in {
    val send = "new x in { x!(12345) }"
    success(send)
    val beforeError = storageContents()
    assert(beforeError.contains("!(12345)"))
    failure("new x, y in { x!(1) | y!(3.noSuchMethod()) }")
    assert(storageContents() == beforeError)
    success("new stdout(`rho:io:stdout`) in { stdout!(42) }")
    assert(storageContents() == beforeError)
  }

  private def storageContents(): String =
    StoragePrinter.prettyPrint(runtime.space.store)

  private def success(rho: String): Unit =
    execute(rho).swap.foreach(error => fail(s"""Execution failed for: $rho
                                               |Cause:
                                               |$error""".stripMargin))

  private def failure(rho: String): Throwable =
    execute(rho).swap.getOrElse(fail(s"Expected $rho to fail - it didn't."))

  private def execute(source: String): Either[Throwable, Runtime] =
    Interpreter
      .execute(runtime, new StringReader(source))
      .attempt
      .runSyncUnsafe(maxDuration)

}
