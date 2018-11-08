package coop.rchain.rholang

import java.io.StringReader

import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.interpreter.{Interpreter, Runtime}
import monix.execution.Scheduler.Implicits.global
import coop.rchain.rholang.Resources.mkRuntime
import monix.eval.Task
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class InterpreterSpec extends FlatSpec with Matchers {
  private val mapSize     = 10L * 1024L * 1024L
  private val tmpPrefix   = "rspace-store-"
  private val maxDuration = 5.seconds

  behavior of "Interpreter"

  it should "restore RSpace to its prior state after evaluation error" in {
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

  it should "yield correct results for the PrimeCheck contract" in {
    success("""
        |new loop, primeCheck, stdoutAck(`rho:io:stdoutAck`) in {
        |            contract loop(@x) = {
        |              match x {
        |                [] => Nil
        |                [head ...tail] => {
        |                  new ret in {
        |                    for (_ <- ret) {
        |                      loop!(tail)
        |                    } | primeCheck!(head, *ret)
        |                  }
        |                }
        |              }
        |            } |
        |            contract primeCheck(@x, ret) = {
        |              match x {
        |                Nil => { stdoutAck!("Nil", *ret) | @0!("Nil") }
        |                ~{~Nil | ~Nil} => { stdoutAck!("Prime", *ret) | @0!("Pr") }
        |                _ => { stdoutAck!("Composite", *ret) |  @0!("Co") }
        |              }
        |            } |
        |            loop!([Nil, 7, 7 | 8, 9 | Nil, 9 | 10, Nil, 9])
        |  }
      """.stripMargin)
    // TODO: this is not the way we should be testing execution results,
    // yet strangely it works - and we don't have a better way for now
    assert(
      storageContents().startsWith(
        Seq(
          """@{0}!("Nil") |""",
          """@{0}!("Pr") |""",
          """@{0}!("Co") |""",
          """@{0}!("Pr") |""",
          """@{0}!("Co") |""",
          """@{0}!("Nil") |""",
          """@{0}!("Pr") |"""
        ).mkString("\n")
      )
    )
  }

  private def storageContents(runtime : Runtime): String =
    StoragePrinter.prettyPrint(runtime.space.store)

  private def success(rho: String): Unit =
    execute(rho).swap.foreach(error => fail(s"""Execution failed for: $rho
                                               |Cause:
                                               |$error""".stripMargin))

  private def failure(rho: String): Throwable =
    execute(rho).swap.getOrElse(fail(s"Expected $rho to fail - it didn't."))

  private def execute(source: String): Either[Throwable, Runtime] =
    mkRuntime[Task](tmpPrefix, mapSize)
      .use{runtime => Interpreter.execute(runtime, new StringReader(source)).attempt}
      .runSyncUnsafe(maxDuration)

}
