package coop.rchain.rholang

import java.io.StringReader
import java.nio.file.Files

import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.interpreter.{Interpreter, Runtime}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.Await
import scala.concurrent.duration._

class InterpreterSpec extends FlatSpec with Matchers {
  val mapSize     = 1024L * 1024L * 1024L
  val tmpPrefix   = "rspace-store-"
  val maxDuration = 5.seconds

  val runtime = Runtime.create(Files.createTempDirectory(tmpPrefix), mapSize)

  "Interpreter" should "restore RSpace to its prior state after evaluation error" in {
    val initStorage = storageContents()
    val send        = "@{0}!(0)"
    success(send)
    val beforeError = storageContents()
    assert(beforeError.contains(send))
    failure("@1!(1) | @2!(3.noSuchMethod())")
    assert(storageContents() == beforeError)
    success("new stdout(`rho:io:stdout`) in { stdout!(42) }")
    assert(storageContents() == beforeError)
    success("for (_ <- @0) { Nil }")
    assert(storageContents() == initStorage)
  }

  private def storageContents() =
    StoragePrinter.prettyPrint(runtime.space.store)

  private def success(rho: String): Unit =
    execute(rho).swap.foreach(error => fail(s"""Execution failed for: $rho
                                               |Cause:
                                               |$error""".stripMargin))

  private def failure(rho: String): Throwable =
    execute(rho).swap.getOrElse(fail(s"Expected $rho to fail - it didn't."))

  private def execute(source: String) = {
    val future = Interpreter.execute(runtime, new StringReader(source)).attempt.runAsync
    Await.result(future, maxDuration)
  }

}
