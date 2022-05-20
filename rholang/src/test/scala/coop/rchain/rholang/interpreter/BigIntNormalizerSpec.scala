package coop.rchain.rholang.interpreter

import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.Resources.mkRuntime
import coop.rchain.rholang.interpreter.errors.{InterpreterError, SyntaxError}
import coop.rchain.rholang.syntax._
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.duration._

class BigIntNormalizerSpec extends AnyWordSpec with Matchers {
  implicit val logF: Log[Task]            = Log.log[Task]
  implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
  implicit val noopSpan: Span[Task]       = NoopSpan[Task]()
  private val maxDuration                 = 5.seconds

  val outcomeCh = "ret"

  private def execute(source: String): Task[Either[InterpreterError, BigInt]] =
    mkRuntime[Task]("rholang-bigint")
      .use { runtime =>
        for {
          evalResult <- runtime.evaluate(source)
          result <- if (evalResult.errors.isEmpty)
                     for {
                       data         <- runtime.getData(GString(outcomeCh)).map(_.head)
                       bigIntResult = data.a.pars.head.exprs.head.getGBigInt
                     } yield Right(bigIntResult)
                   else Task.pure(Left(evalResult.errors.head))
        } yield result
      }

  "method toBigInt()" should {
    "convert Rholang Int value to BigInt" in {
      val termWithNull =
        s"""
           # @"$outcomeCh"!(0.toBigInt())
           # """.stripMargin('#')
      execute(termWithNull).runSyncUnsafe(maxDuration) should equal(
        Right(BigInt(0))
      )
      val termWithMaxLong =
        s"""
           # @"$outcomeCh"!(9223372036854775807.toBigInt())
           # """.stripMargin('#')
      execute(termWithMaxLong).runSyncUnsafe(maxDuration) should equal(
        Right(BigInt(9223372036854775807L))
      )
      val termWithMinLong =
        s"""
           # @"$outcomeCh"!((-9223372036854775807).toBigInt())
           # """.stripMargin('#')
      execute(termWithMinLong).runSyncUnsafe(maxDuration) should equal(
        Right(BigInt(-9223372036854775807L))
      )
    }

    "convert Rholang String to BigInt" in {
      val termWithNull =
        s"""
           # @"$outcomeCh"!("0".toBigInt())
           # """.stripMargin('#')
      execute(termWithNull).runSyncUnsafe(maxDuration) should equal(
        Right(BigInt("0"))
      )
      val termWithPositiveBigValue =
        s"""
           # @"$outcomeCh"!("9999999999999999999999999999999999999999999999".toBigInt())
           # """.stripMargin('#')
      execute(termWithPositiveBigValue).runSyncUnsafe(maxDuration) should equal(
        Right(BigInt("9999999999999999999999999999999999999999999999"))
      )
      val termWithNegativeBigValue =
        s"""
           # @"$outcomeCh"!("-9999999999999999999999999999999999999999999999".toBigInt())
           # """.stripMargin('#')
      execute(termWithNegativeBigValue).runSyncUnsafe(maxDuration) should equal(
        Right(BigInt("-9999999999999999999999999999999999999999999999"))
      )
    }
  }

  "BigInt() constructor" should {
    "create BigInt value" in {
      val termWithNull =
        s"""
           # @"$outcomeCh"!( BigInt(0) )
           # """.stripMargin('#')
      execute(termWithNull).runSyncUnsafe(maxDuration) should equal(
        Right(BigInt("0"))
      )
      val termWithPositiveBigValue =
        s"""
           # @"$outcomeCh"!( BigInt( 9999999999999999999999999999999999999999999999 ) )
           # """.stripMargin('#')
      execute(termWithPositiveBigValue).runSyncUnsafe(maxDuration) should equal(
        Right(BigInt("9999999999999999999999999999999999999999999999"))
      )
      val termWithNegativeBigValue =
        s"""
           # @"$outcomeCh"!( -BigInt(9999999999999999999999999999999999999999999999) )
           # """.stripMargin('#')
      execute(termWithNegativeBigValue).runSyncUnsafe(maxDuration) should equal(
        Right(BigInt("-9999999999999999999999999999999999999999999999"))
      )
    }

    "return throw error if input data isn't number string" in {
      val term1 =
        s"""
         # @"$outcomeCh"!(BigInt(NOTNUMBER))
         # """.stripMargin('#')
      execute(term1).runSyncUnsafe(maxDuration) should equal(
        Left(
          SyntaxError(
            "syntax error(): NOTNUMBER at 2:17-2:26"
          )
        )
      )
      val term2 =
        s"""
         # @"$outcomeCh"!(BigInt(9999999999999999999999999999999999999999999999NOTNUMBER))
         # """.stripMargin('#')
      execute(term2).runSyncUnsafe(maxDuration) should equal(
        Left(
          SyntaxError(
            "syntax error(): NOTNUMBER at 2:63-2:72"
          )
        )
      )
      val term3 =
        s"""
         # @"$outcomeCh"!(BigInt(9999999999999999999999999999999999999999999999 NOTNUMBER))
         # """.stripMargin('#')
      execute(term3).runSyncUnsafe(maxDuration) should equal(
        Left(
          SyntaxError(
            "syntax error(): NOTNUMBER at 2:64-2:73"
          )
        )
      )
    }
  }

}
