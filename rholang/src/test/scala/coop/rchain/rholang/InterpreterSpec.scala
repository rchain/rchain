package coop.rchain.rholang

import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Expr.ExprInstance.{GInt, GString}
import coop.rchain.models.{Expr, Par}
import coop.rchain.rholang.Resources.mkRuntime
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.interpreter.{EvaluateResult, Interpreter, RhoRuntime}
import coop.rchain.rholang.interpreter.syntax._
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class InterpreterSpec extends FlatSpec with Matchers {
  private val mapSize     = 1024L * 1024L * 1024L
  private val tmpPrefix   = "rspace-store-"
  private val maxDuration = 5.seconds

  implicit val logF: Log[Task]            = new Log.NOPLog[Task]
  implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
  implicit val noopSpan: Span[Task]       = NoopSpan[Task]()

  behavior of "Interpreter"

  it should "restore RSpace to its prior state after evaluation error" in {

    val sendRho = "@{0}!(0)"

    mkRuntime[Task](tmpPrefix, mapSize)
      .use { runtime =>
        for {
          initStorage           <- storageContents(runtime)
          _                     <- success(runtime, sendRho)
          beforeError           <- storageContents(runtime)
          _                     = assert(beforeError.contains(sendRho))
          beforeErrorCheckpoint <- runtime.createCheckpoint
          _                     <- failure(runtime, "@1!(1) | @2!(3.noSuchMethod())")
          afterErrorCheckpoint  <- runtime.createCheckpoint
          _                     = assert(afterErrorCheckpoint.root == beforeErrorCheckpoint.root)
          _                     <- success(runtime, "new stdout(`rho:io:stdout`) in { stdout!(42) }")
          afterSendCheckpoint   <- runtime.createCheckpoint
          _                     = assert(afterSendCheckpoint.root == beforeErrorCheckpoint.root)
          _                     <- success(runtime, "for (_ <- @0) { Nil }")
          finalContent          <- storageContents(runtime)
          _                     = assert(finalContent == initStorage)
        } yield ()
      }
      .runSyncUnsafe(maxDuration)
  }

  it should "yield correct results for the PrimeCheck contract" in {
    val tupleSpace = mkRuntime[Task](tmpPrefix, mapSize)
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

          tupleSpace <- runtime.getHotChanges
        } yield tupleSpace
      }
      .runSyncUnsafe(maxDuration)

    def rhoPar(e: Expr)      = Seq(Par(exprs = Seq(e)))
    def rhoInt(n: Long)      = rhoPar(Expr(GInt(n)))
    def rhoString(s: String) = rhoPar(Expr(GString(s)))

    // Get values on zero channel
    val chZero  = rhoInt(0)
    val results = tupleSpace(chZero).data.map(x => x.a.pars)

    // Expected values
    val expected = Seq("Nil", "Nil", "Pr", "Pr", "Pr", "Co", "Co") map rhoString

    results.toSet shouldBe expected.toSet
  }

  it should "signal syntax errors to the caller" in {
    val badRholang = "new f, x in { f(x) }"
    val EvaluateResult(_, errors) =
      mkRuntime[Task](tmpPrefix, mapSize)
        .use { runtime =>
          execute(runtime, badRholang)
        }
        .runSyncUnsafe(maxDuration)

    errors should not be empty
    errors(0) shouldBe a[coop.rchain.rholang.interpreter.errors.SyntaxError]
  }

  it should "capture rholang parsing errors and charge for parsing" in {
    val badRholang = """ for(@x <- @"x"; @y <- @"y"){ @"xy"!(x + y) | @"x"!(1) | @"y"!("hi") """
    val EvaluateResult(cost, errors) =
      mkRuntime[Task](tmpPrefix, mapSize)
        .use { runtime =>
          execute(runtime, badRholang)
        }
        .runSyncUnsafe(maxDuration)

    errors should not be empty
    cost.value shouldEqual (parsingCost(badRholang).value)
  }

  it should "charge for parsing even when there's not enough phlo to complete it" in {
    val sendRho     = "@{0}!(0)"
    val initialPhlo = parsingCost(sendRho) - Cost(1)
    val EvaluateResult(cost, errors) =
      mkRuntime[Task](tmpPrefix, mapSize)
        .use { runtime =>
          runtime.evaluate(sendRho, initialPhlo)
        }
        .runSyncUnsafe(maxDuration)

    errors should not be empty
    cost.value shouldEqual initialPhlo.value
  }

  private def storageContents(runtime: RhoRuntime[Task]): Task[String] =
    StoragePrinter.prettyPrint(runtime)

  private def success(runtime: RhoRuntime[Task], rho: String): Task[Unit] =
    execute(runtime, rho).map(
      res =>
        assert(
          res.errors.isEmpty,
          s"""Execution failed for: $rho
              |Cause:
              |${res.errors}""".stripMargin
        )
    )

  private def failure(runtime: RhoRuntime[Task], rho: String): Task[Unit] =
    execute(runtime, rho).map(
      res => assert(res.errors.nonEmpty, s"Expected $rho to fail - it didn't.")
    )

  private def execute(
      runtime: RhoRuntime[Task],
      source: String
  ): Task[EvaluateResult] =
    runtime.evaluate(source)

}
