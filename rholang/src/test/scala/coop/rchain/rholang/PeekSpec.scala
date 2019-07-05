package coop.rchain.rholang

import org.scalatest._

import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Expr.ExprInstance.{GInt, GString}
import coop.rchain.models.rholang.implicits._
import coop.rchain.shared.Log
import coop.rchain.rholang.interpreter.{EvaluateResult, InterpreterUtil, Runtime}

import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.duration._

class PeekSpec extends FlatSpec with Matchers {

  import Resources._
  import InterpreterUtil._

  implicit val logF: Log[Task]            = new Log.NOPLog[Task]
  implicit val noopMetrics: Metrics[Task] = new Metrics.MetricsNOP[Task]
  implicit val noopSpan: Span[Task]       = NoopSpan[Task]()

  val tmpPrefix = "peek-spec-"

  "peek" should "not remove read data" in {
    mkRuntime[Task](tmpPrefix)
      .use { runtime =>
        implicit val c = runtime.cost
        for {
          res1 <- evaluate[Task](runtime, """@1!("v1") | for(_ <<- @1) { Nil }""")
          _    = res1.errors shouldBe empty
          res2 <- evaluate[Task](runtime, """for(_ <- @1) { @2!("v2") }""")
          _    = res2.errors shouldBe empty
          data <- runtime.space.getData(GInt(2L))
          _ = withClue(
            "Continuation didn't produce expected data. Did it fire?"
          ) { data should have size 1 }
        } yield (data.head.a.pars.head.exprs.head.exprInstance shouldBe GString("v2"))
      }
      .runSyncUnsafe(2.seconds)
  }

  it should "not duplicate read persistent data - send is executed first" in {
    mkRuntime[Task](tmpPrefix)
      .use { runtime =>
        implicit val c = runtime.cost
        for {
          res1       <- evaluate[Task](runtime, """@1!!("v1")""")
          _          = res1.errors shouldBe empty
          res1a      <- evaluate[Task](runtime, """for(_ <<- @1) { Nil }""")
          _          = res1a.errors shouldBe empty
          res2       <- evaluate[Task](runtime, """for(_ <- @1) { @2!("v2") }""")
          _          = res2.errors shouldBe empty
          v1Data     <- runtime.space.getData(GInt(1L))
          _          = v1Data should have size 1
          resultData <- runtime.space.getData(GInt(2L))
          _ = withClue(
            "Continuation didn't produce expected data. Did it fire?"
          ) { resultData should have size 1 }
        } yield (resultData.head.a.pars.head.exprs.head.exprInstance shouldBe GString("v2"))
      }
      .runSyncUnsafe(2.seconds)
  }

  it should "not duplicate read persistent data - send is executed second" in {
    mkRuntime[Task](tmpPrefix)
      .use { runtime =>
        implicit val c = runtime.cost
        for {
          res1       <- evaluate[Task](runtime, """for(_ <<- @1) { Nil }""")
          _          = res1.errors shouldBe empty
          res1a      <- evaluate[Task](runtime, """@1!!("v1")""")
          _          = res1a.errors shouldBe empty
          res2       <- evaluate[Task](runtime, """for(_ <- @1) { @2!("v2") }""")
          _          = res2.errors shouldBe empty
          v1Data     <- runtime.space.getData(GInt(1L))
          _          = v1Data should have size 1
          resultData <- runtime.space.getData(GInt(2L))
          _ = withClue(
            "Continuation didn't produce expected data. Did it fire?"
          ) { resultData should have size 1 }
        } yield (resultData.head.a.pars.head.exprs.head.exprInstance shouldBe GString("v2"))
      }
      .runSyncUnsafe(2.seconds)
  }
}
