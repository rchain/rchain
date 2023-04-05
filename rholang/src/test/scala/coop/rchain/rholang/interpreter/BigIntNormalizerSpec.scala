package coop.rchain.rholang.interpreter

import cats.Parallel
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{Concurrent, IO}
import cats.syntax.all._
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.Resources.mkRuntime
import coop.rchain.rholang.interpreter.errors.{InterpreterError, SyntaxError}
import coop.rchain.rholang.syntax._
import coop.rchain.shared.Log
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

// TODO enable when CE is migrated to 3 (cats.effect.testing.scalatest is not available for CE2)
//class BigIntNormalizerSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers {
//  implicit val logF: Log[IO]            = Log.log[IO]
//  implicit val noopMetrics: Metrics[IO] = new metrics.Metrics.MetricsNOP[IO]
//  implicit val noopSpan: Span[IO]       = NoopSpan[IO]()
//
//  import coop.rchain.shared.RChainScheduler._
//  val outcomeCh = "ret"
//
//  private def execute[F[_]: Concurrent: Parallel: ContextShift: Metrics: Span: Log](
//      source: String
//  ): F[Either[InterpreterError, BigInt]] =
//    mkRuntime[F]("rholang-bigint")
//      .use { runtime =>
//        for {
//          evalResult <- runtime.evaluate(source)
//          result <- if (evalResult.errors.isEmpty)
//                     for {
//                       data         <- runtime.getData(GString(outcomeCh)).map(_.head)
//                       bigIntResult = data.a.pars.head.exprs.head.getGBigInt
//                     } yield Right(bigIntResult)
//                   else Left(evalResult.errors.head).pure[F]
//        } yield result
//      }
//
//  "method toBigInt()" should "convert Rholang Int value to BigInt" in {
//    val termWithNull =
//      s"""
//           # @"$outcomeCh"!(0.toBigInt())
//           # """.stripMargin('#')
//    val termWithMaxLong =
//      s"""
//           # @"$outcomeCh"!(9223372036854775807.toBigInt())
//           # """.stripMargin('#')
//    val termWithMinLong =
//      s"""
//           # @"$outcomeCh"!((-9223372036854775807).toBigInt())
//           # """.stripMargin('#')
//    (for {
//      r1 <- execute[IO](termWithNull)
//      r2 <- execute[IO](termWithMaxLong)
//      r3 <- execute[IO](termWithMinLong)
//    } yield {
//      r1 should equal(Right(BigInt(0)))
//      r2 should equal(Right(BigInt(9223372036854775807L)))
//      r3 should equal(Right(BigInt(-9223372036854775807L)))
//    }).unsafeToFuture()
//  }
//
//  it should "convert Rholang String to BigInt" in {
//    val termWithNull =
//      s"""
//         # @"$outcomeCh"!("0".toBigInt())
//         # """.stripMargin('#')
//    val termWithPositiveBigValue =
//      s"""
//         # @"$outcomeCh"!("9999999999999999999999999999999999999999999999".toBigInt())
//         # """.stripMargin('#')
//    val termWithNegativeBigValue =
//      s"""
//         # @"$outcomeCh"!("-9999999999999999999999999999999999999999999999".toBigInt())
//         # """.stripMargin('#')
//    (for {
//      r1 <- execute[IO](termWithNull)
//      r2 <- execute[IO](termWithPositiveBigValue)
//      r3 <- execute[IO](termWithNegativeBigValue)
//    } yield {
//      r1 should equal(Right(BigInt("0")))
//      r2 should equal(Right(BigInt("9999999999999999999999999999999999999999999999")))
//      r3 should equal(Right(BigInt("-9999999999999999999999999999999999999999999999")))
//    }).unsafeToFuture()
//  }
//
//  "BigInt() constructor" should "create BigInt value" in {
//    val termWithNull =
//      s"""
//           # @"$outcomeCh"!( BigInt(0) )
//           # """.stripMargin('#')
//    val termWithPositiveBigValue =
//      s"""
//           # @"$outcomeCh"!( BigInt( 9999999999999999999999999999999999999999999999 ) )
//           # """.stripMargin('#')
//    val termWithNegativeBigValue =
//      s"""
//           # @"$outcomeCh"!( -BigInt(9999999999999999999999999999999999999999999999) )
//           # """.stripMargin('#')
//    (for {
//      r1 <- execute[IO](termWithNull)
//      r2 <- execute[IO](termWithPositiveBigValue)
//      r3 <- execute[IO](termWithNegativeBigValue)
//    } yield {
//      r1 should equal(Right(BigInt("0")))
//      r2 should equal(Right(BigInt("9999999999999999999999999999999999999999999999")))
//      r3 should equal(Right(BigInt("-9999999999999999999999999999999999999999999999")))
//    }).unsafeToFuture()
//  }
//
//  it should "return throw error if input data isn't number string or it is negative number" in {
//    val term1 =
//      s"""
//         # @"$outcomeCh"!(BigInt(NOTNUMBER))
//         # """.stripMargin('#')
//    val term2 =
//      s"""
//         # @"$outcomeCh"!(BigInt(9999999999999999999999999999999999999999999999NOTNUMBER))
//         # """.stripMargin('#')
//    val term3 =
//      s"""
//         # @"$outcomeCh"!(BigInt(9999999999999999999999999999999999999999999999 NOTNUMBER))
//         # """.stripMargin('#')
//    val term4 =
//      s"""
//         # @"$outcomeCh"!(BigInt(-9999999999999999999999999999999999999999999999))
//         # """.stripMargin('#')
//    (for {
//      r1 <- execute[IO](term1)
//      r2 <- execute[IO](term2)
//      r3 <- execute[IO](term3)
//      r4 <- execute[IO](term4)
//    } yield {
//      r1 should equal(Left(SyntaxError("syntax error(): NOTNUMBER at 2:17-2:26")))
//      r2 should equal(Left(SyntaxError("syntax error(): NOTNUMBER at 2:63-2:72")))
//      r3 should equal(Left(SyntaxError("syntax error(): NOTNUMBER at 2:64-2:73")))
//      r4 should equal(Left(SyntaxError("syntax error(): - at 2:17-2:18")))
//    }).unsafeToFuture()
//  }
//}
