package coop.rchain.rholang

import java.io.StringReader

import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.interpreter.{Interpreter, Runtime}
import monix.execution.Scheduler.Implicits.global
import coop.rchain.rholang.Resources.mkRuntime
import monix.eval.Task
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._
import scala.util.Try

class InterpreterSpec extends FlatSpec with Matchers {
  private val mapSize     = 10L * 1024L * 1024L
  private val tmpPrefix   = "rspace-store-"
  private val maxDuration = 5.seconds

  behavior of "Interpreter"

  it should "restore RSpace to its prior state after evaluation error" in {
    import coop.rchain.catscontrib.effect.implicits.bracketTry

    val sendRho = "@{0}!(0)"

    val (initStorage, beforeError, afterError, afterSend, finalContent) =
      mkRuntime(tmpPrefix, mapSize)
        .use { runtime =>
          val initStorage = storageContents(runtime)
          for {
            _            <- success(runtime, sendRho)
            beforeError  = storageContents(runtime)
            _            <- failure(runtime, "@1!(1) | @2!(3.noSuchMethod())")
            afterError   = storageContents(runtime)
            _            <- success(runtime, "new stdout(`rho:io:stdout`) in { stdout!(42) }")
            afterSend    = storageContents(runtime)
            _            <- success(runtime, "for (_ <- @0) { Nil }")
            finalContent = storageContents(runtime)
          } yield (initStorage, beforeError, afterError, afterSend, finalContent)
        }
        .runSyncUnsafe(maxDuration)

    assert(beforeError.contains(sendRho))
    assert(afterError == beforeError)
    assert(afterSend == beforeError)
    assert(finalContent == initStorage)
  }

  it should "yield correct results for the PrimeCheck contract" in {
    import coop.rchain.catscontrib.effect.implicits.bracketTry
    val contents = mkRuntime(tmpPrefix, mapSize)
      .use { runtime =>
        for {
          _ <- success(
                runtime,
                """
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
            """.stripMargin
              )

          contents = storageContents(runtime)
        } yield contents
      }
      .runSyncUnsafe(maxDuration)

    // TODO: this is not the way we should be testing execution results,
    // yet strangely it works - and we don't have a better way for now
    assert(
      contents.startsWith(
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

  private def storageContents(runtime: Runtime): String =
    StoragePrinter.prettyPrint(runtime.space.store)

  private def success(runtime: Runtime, rho: String): Task[Unit] =
    execute(runtime, rho).map(_.swap.foreach(error => fail(s"""Execution failed for: $rho
                                               |Cause:
                                               |$error""".stripMargin)))

  private def failure(runtime: Runtime, rho: String): Task[Throwable] =
    execute(runtime, rho).map(_.swap.getOrElse(fail(s"Expected $rho to fail - it didn't.")))

  private def execute(runtime: Runtime, source: String): Task[Either[Throwable, Runtime]] =
    Interpreter.execute(runtime, new StringReader(source)).attempt

}
