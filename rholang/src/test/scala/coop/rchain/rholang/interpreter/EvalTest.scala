package coop.rchain.rholang.interpreter

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models.Par
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.Resources.mkRuntime
import coop.rchain.rholang.interpreter.compiler.Compiler
import coop.rchain.rholang.syntax._
import coop.rchain.shared.Log
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class EvalTest extends AnyWordSpec with Matchers {
  implicit val logF: Log[IO]            = Log.log[IO]
  implicit val noopMetrics: Metrics[IO] = new metrics.Metrics.MetricsNOP[IO]
  implicit val noopSpan: Span[IO]       = NoopSpan[IO]()

  val outcomeCh      = "ret"
  val reduceErrorMsg = "Error: index out of bound: -1"

  private def execute(source: String): IO[Par] =
    mkRuntime[IO]("rholang-eval-spec")
      .use { runtime =>
        for {
          _    <- runtime.evaluate(source)
          data <- runtime.getData(GString(outcomeCh)).map(_.head)
        } yield data.a.pars.head
      }

  "runtime" should {
    "convert term to Par and evalue it" in {
//      val term = """{ "key11"|"key12":"data1", "key2":"data2"}"""
      val term = """ new x, y in { *x + *y } """
      val ast  = Compiler[IO].sourceToADT(term).unsafeRunSync()
      println("AST:")
      println(ast)
      println("prettyAST:")
      println(PrettyPrinter().buildString(ast))

      val term2    = s"""@"$outcomeCh"!($term)"""
      val evalTerm = execute(term2).unsafeRunSync()
      println("evalTerm:")
      println(evalTerm)

      val term3         = s""" @"chan"!( $term ) | for(@q <- @"chan") { @"$outcomeCh"!(q) } """
      val processedTerm = execute(term3).unsafeRunSync()
      println("processedTerm:")
      println(processedTerm)
    }
  }
}
