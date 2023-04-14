package coop.rchain.rholang.interpreter

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.rholang.Resources.mkRuntime
import coop.rchain.shared.Log
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import coop.rchain.rholang.syntax._

import scala.concurrent.duration._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.errors.{InterpreterError, ReduceError}

class ShortCircuitBooleanSpec extends AnyWordSpec with Matchers {

  implicit val logF: Log[IO]            = Log.log[IO]
  implicit val noopMetrics: Metrics[IO] = new metrics.Metrics.MetricsNOP[IO]
  implicit val noopSpan: Span[IO]       = NoopSpan[IO]()

  val outcomeCh      = "ret"
  val reduceErrorMsg = "Error: index out of bound: -1"

  private def execute(source: String): IO[Either[InterpreterError, Boolean]] =
    mkRuntime[IO]("rholang-short-circuit-boolean")
      .use { runtime =>
        for {
          evalResult <- runtime.evaluate(source)
          result <- if (evalResult.errors.isEmpty)
                     for {
                       data       <- runtime.getData(GString(outcomeCh)).map(_.head)
                       boolResult = data.a.pars.head.exprs.head.getGBool
                     } yield Right(boolResult)
                   else IO.pure(Left(evalResult.errors.head))
        } yield result
      }

  " `par1 && par2` rholang boolean evaluation" should {
    "execute only the first par1 if par1==false" in {
      val term =
        s"""
            # @"${outcomeCh}"!(false && [1,2].nth(-1))
            # """.stripMargin('#')
      execute(term).unsafeRunSync should equal(Right(false))

      val term2 =
        s"""
            # @"${outcomeCh}"!(1 < 0 && [1,2].nth(-1))
            # """.stripMargin('#')
      execute(term2).unsafeRunSync should equal(Right(false))
    }
    "execute both par1 and par2 if par1 == true" in {
      val term =
        s"""
           # @"${outcomeCh}"!(true && [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term).unsafeRunSync should equal(Left(ReduceError(reduceErrorMsg)))

      val term2 =
        s"""
           # @"${outcomeCh}"!(1>0 && [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term2).unsafeRunSync should equal(Left(ReduceError(reduceErrorMsg)))
    }
  }

  " `par1` || par2` rholang boolean evaluation" should {
    "evaluate only the first par1 if par1==true" in {
      val term =
        s"""
           # @"${outcomeCh}"!(true || [1,2].nth(-1))
           # """.stripMargin('#')
      execute(term).unsafeRunSync should equal(Right(true))

      val term2 =
        s"""
           # @"${outcomeCh}"!(1 > 0 || [1,2].nth(-1))
           # """.stripMargin('#')
      execute(term2).unsafeRunSync should equal(Right(true))
    }
    "evaluate both par1 and par2 if par1 == false" in {
      val term =
        s"""
           # @"${outcomeCh}"!(false || [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term).unsafeRunSync should equal(Left(ReduceError(reduceErrorMsg)))

      val term2 =
        s"""
           # @"${outcomeCh}"!(1<0 || [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term2).unsafeRunSync should equal(Left(ReduceError(reduceErrorMsg)))
    }
  }

  "&&" should {
    "work with the same precedence with `and`" in {
      val term =
        s"""
           # @"${outcomeCh}"!(false && 1>0 and [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term).unsafeRunSync should equal(Left(ReduceError(reduceErrorMsg)))

      val term2 =
        s"""
           # @"${outcomeCh}"!(false and 1>0 && [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term2).unsafeRunSync should equal(Right(false))

    }

    "work with higher precedence with `or`" in {
      val term =
        s"""
           # @"${outcomeCh}"!(false && 1>0 or [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term).unsafeRunSync should equal(Left(ReduceError(reduceErrorMsg)))

      val term2 =
        s"""
           # @"${outcomeCh}"!(false or 1<0 && [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term2).unsafeRunSync should equal(Right(false))

    }
  }

  "||" should {
    "work with lower precedence with `and`" in {
      val term =
        s"""
           # @"${outcomeCh}"!(false || 1>0 and [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term).unsafeRunSync should equal(Left(ReduceError(reduceErrorMsg)))

      val term2 =
        s"""
           # @"${outcomeCh}"!(true and 1>0 || [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term2).unsafeRunSync should equal(Right(true))
    }

    "work with the same precedence with `or`" in {
      val term =
        s"""
           # @"${outcomeCh}"!(false || 1>0 or [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term).unsafeRunSync should equal(Left(ReduceError(reduceErrorMsg)))

      val term2 =
        s"""
           # @"${outcomeCh}"!(true or 1>0 || [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term2).unsafeRunSync should equal(Right(true))
    }
  }

  "&& and ||" should {
    "substitute right for bound var in embedded scope" in {
      // If substituting don't work in && (or ||) - x1 would be unbound
      val term = s""" new ret1, ret2 in {
                    #    ret1!(false) |
                    #    for(@x1 <- ret1) {
                    #        ret2!(Nil)|
                    #        for (_ <- ret2) {
                    #            @"${outcomeCh}"!(true && x1)
                    #        }
                    #    }
                    #}""".stripMargin('#')

      execute(term).unsafeRunSync should equal(Right(false))

      val term2 = s""" new ret1, ret2 in {
                     #    ret1!(true) |
                     #    for(@x1 <- ret1) {
                     #        ret2!(Nil)|
                     #        for (_ <- ret2){
                     #          @"${outcomeCh}"!(false || x1)
                     #        }
                     #    }
                     #
                     #}""".stripMargin('#')

      execute(term2).unsafeRunSync should equal(Right(true))
    }
  }
}
