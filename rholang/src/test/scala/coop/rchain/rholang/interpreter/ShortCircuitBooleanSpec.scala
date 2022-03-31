package coop.rchain.rholang.interpreter

import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.rholang.Resources.mkRuntime
import coop.rchain.shared.Log
import monix.eval.{Coeval, Task}
import org.scalatest.{Matchers, WordSpec}
import coop.rchain.rholang.syntax._

import scala.concurrent.duration._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.compiler.Compiler
import coop.rchain.rholang.interpreter.errors.{InterpreterError, ReduceError}
import monix.execution.Scheduler.Implicits.global

class ShortCircuitBooleanSpec extends WordSpec with Matchers {
  implicit val logF: Log[Task]            = Log.log[Task]
  implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
  implicit val noopSpan: Span[Task]       = NoopSpan[Task]()
  private val maxDuration                 = 5.seconds

  val outcomeCh      = "ret"
  val reduceErrorMsg = "Error: index out of bound: -1"

  private def execute(source: String): Task[Either[InterpreterError, Boolean]] =
    mkRuntime[Task]("rholang-short-circuit-boolean")
      .use { runtime =>
        for {
          evalResult <- runtime.evaluate(source)
          result <- if (evalResult.errors.isEmpty)
                     for {
                       data       <- runtime.getData(GString(outcomeCh)).map(_.head)
                       boolResult = data.a.pars.head.exprs.head.getGBool
                     } yield Right(boolResult)
                   else Task.pure(Left(evalResult.errors.head))
        } yield result
      }

  " `par1 && par2` rholang boolean evaluation" should {
    "execute only the first par1 if par1==false" in {
      val term =
        s"""
            # @"${outcomeCh}"!(false && [1,2].nth(-1))
            # """.stripMargin('#')
      execute(term).runSyncUnsafe(maxDuration) should equal(Right(false))

      val term2 =
        s"""
            # @"${outcomeCh}"!(1 < 0 && [1,2].nth(-1))
            # """.stripMargin('#')
      execute(term2).runSyncUnsafe(maxDuration) should equal(Right(false))
    }
    "execute both par1 and par2 if par1 == true" in {
      val term =
        s"""
           # @"${outcomeCh}"!(true && [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term).runSyncUnsafe(maxDuration) should equal(Left(ReduceError(reduceErrorMsg)))

      val term2 =
        s"""
           # @"${outcomeCh}"!(1>0 && [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term2).runSyncUnsafe(maxDuration) should equal(Left(ReduceError(reduceErrorMsg)))
    }
  }

  " `par1` || par2` rholang boolean evaluation" should {
    "evaluate only the first par1 if par1==true" in {
      val term =
        s"""
           # @"${outcomeCh}"!(true || [1,2].nth(-1))
           # """.stripMargin('#')
      execute(term).runSyncUnsafe(maxDuration) should equal(Right(true))

      val term2 =
        s"""
           # @"${outcomeCh}"!(1 > 0 || [1,2].nth(-1))
           # """.stripMargin('#')
      execute(term2).runSyncUnsafe(maxDuration) should equal(Right(true))
    }
    "evaluate both par1 and par2 if par1 == false" in {
      val term =
        s"""
           # @"${outcomeCh}"!(false || [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term).runSyncUnsafe(maxDuration) should equal(Left(ReduceError(reduceErrorMsg)))

      val term2 =
        s"""
           # @"${outcomeCh}"!(1<0 || [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term2).runSyncUnsafe(maxDuration) should equal(Left(ReduceError(reduceErrorMsg)))
    }
  }

  "&&" should {
    "work good with `and`" in {
      val term =
        s"""
           # @"${outcomeCh}"!(false && 1>0 and [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term).runSyncUnsafe(maxDuration) should equal(Left(ReduceError(reduceErrorMsg)))

      val term2 =
        s"""
           # @"${outcomeCh}"!(false and 1>0 && [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term2).runSyncUnsafe(maxDuration) should equal(Right(false))

    }

    "work good with `or`" in {
      val term =
        s"""
           # @"${outcomeCh}"!(false && 1>0 or [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term).runSyncUnsafe(maxDuration) should equal(Left(ReduceError(reduceErrorMsg)))

      val term2 =
        s"""
           # @"${outcomeCh}"!(false or 1<0 && [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term2).runSyncUnsafe(maxDuration) should equal(Right(false))

    }
  }

  "||" should {
    "work good with `and`" in {
      val term =
        s"""
           # @"${outcomeCh}"!(false || 1>0 and [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term).runSyncUnsafe(maxDuration) should equal(Left(ReduceError(reduceErrorMsg)))

      val term2 =
        s"""
           # @"${outcomeCh}"!(true and 1>0 || [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term2).runSyncUnsafe(maxDuration) should equal(Right(true))
    }

    "work good with `or`" in {
      val term =
        s"""
           # @"${outcomeCh}"!(false || 1>0 or [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term).runSyncUnsafe(maxDuration) should equal(Left(ReduceError(reduceErrorMsg)))

      val term2 =
        s"""
           # @"${outcomeCh}"!(true or 1>0 || [1,2].nth(-1))
           # """.stripMargin('#')

      execute(term2).runSyncUnsafe(maxDuration) should equal(Right(true))
    }
  }
}
