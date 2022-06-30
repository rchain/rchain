package coop.rchain.rholang.interpreter

import cats.Parallel
import cats.effect.{Concurrent, ContextShift}
import cats.syntax.all._
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.Resources.mkRuntime
import coop.rchain.rholang.interpreter.errors.{InterpreterError, SyntaxError}
import coop.rchain.rholang.syntax._
import coop.rchain.shared.Log
import monix.eval.Task
import monix.testing.scalatest.MonixTaskTest
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class BigIntNormalizerSpec extends AsyncFlatSpec with MonixTaskTest with Matchers {
  implicit val logF: Log[Task]            = Log.log[Task]
  implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
  implicit val noopSpan: Span[Task]       = NoopSpan[Task]()

  val outcomeCh = "ret"

  private def execute[F[_]: Concurrent: Parallel: ContextShift: Metrics: Span: Log](
      source: String
  ): F[Either[InterpreterError, BigInt]] =
    mkRuntime[F]("rholang-bigint")
      .use { runtime =>
        for {
          evalResult <- runtime.evaluate(source)
          result <- if (evalResult.errors.isEmpty)
                     for {
                       data         <- runtime.getData(GString(outcomeCh)).map(_.head)
                       bigIntResult = data.a.pars.head.exprs.head.getGBigInt
                     } yield Right(bigIntResult)
                   else Left(evalResult.errors.head).pure[F]
        } yield result
      }

  "method toBigInt()" should "convert Rholang Int value to BigInt" in {
    val termWithNull =
      s"""
           # @"$outcomeCh"!(0.toBigInt())
           # """.stripMargin('#')
    val termWithMaxLong =
      s"""
           # @"$outcomeCh"!(9223372036854775807.toBigInt())
           # """.stripMargin('#')
    val termWithMinLong =
      s"""
           # @"$outcomeCh"!((-9223372036854775807).toBigInt())
           # """.stripMargin('#')
    for {
      r1 <- execute[Task](termWithNull)
      r2 <- execute[Task](termWithMaxLong)
      r3 <- execute[Task](termWithMinLong)
    } yield {
      r1 should equal(Right(BigInt(0)))
      r2 should equal(Right(BigInt(9223372036854775807L)))
      r3 should equal(Right(BigInt(-9223372036854775807L)))
    }
  }

  it should "convert Rholang String to BigInt" in {
    val termWithNull =
      s"""
         # @"$outcomeCh"!("0".toBigInt())
         # """.stripMargin('#')
    val termWithPositiveBigValue =
      s"""
         # @"$outcomeCh"!("9999999999999999999999999999999999999999999999".toBigInt())
         # """.stripMargin('#')
    val termWithNegativeBigValue =
      s"""
         # @"$outcomeCh"!("-9999999999999999999999999999999999999999999999".toBigInt())
         # """.stripMargin('#')
    for {
      r1 <- execute[Task](termWithNull)
      r2 <- execute[Task](termWithPositiveBigValue)
      r3 <- execute[Task](termWithNegativeBigValue)
    } yield {
      r1 should equal(Right(BigInt("0")))
      r2 should equal(Right(BigInt("9999999999999999999999999999999999999999999999")))
      r3 should equal(Right(BigInt("-9999999999999999999999999999999999999999999999")))
    }
  }

  "BigInt() constructor" should "create BigInt value" in {
    val termWithNull =
      s"""
           # @"$outcomeCh"!( BigInt(0) )
           # """.stripMargin('#')
    val termWithPositiveBigValue =
      s"""
           # @"$outcomeCh"!( BigInt( 9999999999999999999999999999999999999999999999 ) )
           # """.stripMargin('#')
    val termWithNegativeBigValue =
      s"""
           # @"$outcomeCh"!( -BigInt(9999999999999999999999999999999999999999999999) )
           # """.stripMargin('#')
    for {
      r1 <- execute[Task](termWithNull)
      r2 <- execute[Task](termWithPositiveBigValue)
      r3 <- execute[Task](termWithNegativeBigValue)
    } yield {
      r1 should equal(Right(BigInt("0")))
      r2 should equal(Right(BigInt("9999999999999999999999999999999999999999999999")))
      r3 should equal(Right(BigInt("-9999999999999999999999999999999999999999999999")))
    }
  }

  it should "return throw error if input data isn't number string" in {
    val term1 =
      s"""
         # @"$outcomeCh"!(BigInt(NOTNUMBER))
         # """.stripMargin('#')
    val term2 =
      s"""
         # @"$outcomeCh"!(BigInt(9999999999999999999999999999999999999999999999NOTNUMBER))
         # """.stripMargin('#')
    val term3 =
      s"""
         # @"$outcomeCh"!(BigInt(9999999999999999999999999999999999999999999999 NOTNUMBER))
         # """.stripMargin('#')
    for {
      r1 <- execute[Task](term1)
      r2 <- execute[Task](term2)
      r3 <- execute[Task](term3)
    } yield {
      r1 should equal(Left(SyntaxError("syntax error(): NOTNUMBER at 2:17-2:26")))
      r2 should equal(Left(SyntaxError("syntax error(): NOTNUMBER at 2:63-2:72")))
      r3 should equal(Left(SyntaxError("syntax error(): NOTNUMBER at 2:64-2:73")))
    }
  }
}
