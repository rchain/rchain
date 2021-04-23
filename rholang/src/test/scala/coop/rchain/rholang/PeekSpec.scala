package coop.rchain.rholang

import org.scalatest._

import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Expr.ExprInstance.{GInt, GString}
import coop.rchain.models.rholang.implicits._
import coop.rchain.shared.Log
import coop.rchain.rholang.interpreter.InterpreterUtil

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
        for {
          _    <- evaluate[Task](runtime, """@1!("v1") | for(_ <<- @1) { Nil }""")
          _    <- evaluate[Task](runtime, """for(_ <- @1) { @2!("v2") }""")
          data <- runtime.getData(GInt(2L))
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
        for {
          _          <- evaluate[Task](runtime, """@1!!("v1")""")
          _          <- evaluate[Task](runtime, """for(_ <<- @1) { Nil }""")
          _          <- evaluate[Task](runtime, """for(_ <- @1) { @2!("v2") }""")
          v1Data     <- runtime.getData(GInt(1L))
          _          = v1Data should have size 1
          resultData <- runtime.getData(GInt(2L))
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
        for {
          _          <- evaluate[Task](runtime, """for(_ <<- @1) { Nil }""")
          _          <- evaluate[Task](runtime, """@1!!("v1")""")
          _          <- evaluate[Task](runtime, """for(_ <- @1) { @2!("v2") }""")
          v1Data     <- runtime.getData(GInt(1L))
          _          = v1Data should have size 1
          resultData <- runtime.getData(GInt(2L))
          _ = withClue(
            "Continuation didn't produce expected data. Did it fire?"
          ) { resultData should have size 1 }
        } yield (resultData.head.a.pars.head.exprs.head.exprInstance shouldBe GString("v2"))
      }
      .runSyncUnsafe(2.seconds)
  }

  it should "clear all peeks when inserting a persistent send" in {
    mkRuntime[Task](tmpPrefix)
      .use { runtime =>
        for {
          _   <- evaluate[Task](runtime, """for (_ <<- @0) { @1!(0) }""")
          _   <- evaluate[Task](runtime, """for (_ <<- @0) { @1!(0) }""")
          _   <- evaluate[Task](runtime, """@0!!(0)""")
          res <- runtime.getData(GInt(1L)).map(_.size)
        } yield (res shouldBe 2)
      }
      .runSyncUnsafe(2.seconds)
  }

  it should "clear all peeks when inserting a send" in {
    mkRuntime[Task](tmpPrefix)
      .use { runtime =>
        for {
          _   <- evaluate[Task](runtime, """for (_ <<- @0) { @1!(0) }""")
          _   <- evaluate[Task](runtime, """for (_ <<- @0) { @1!(0) }""")
          _   <- evaluate[Task](runtime, """@0!(0)""")
          res <- runtime.getData(GInt(1L)).map(_.size)
        } yield (res shouldBe 2)
      }
      .runSyncUnsafe(2.seconds)
  }

  it should "continue executing the loop until quiescence" in {
    mkRuntime[Task](tmpPrefix)
      .use { runtime =>
        for {
          _  <- evaluate[Task](runtime, """for (_ <<- @0; _ <<- @1) { @2!(0) }""")
          _  <- evaluate[Task](runtime, """for (_ <<- @0; _ <<- @1) { @2!(0) }""")
          _  <- evaluate[Task](runtime, """@1!!(1)""")
          _  <- evaluate[Task](runtime, """@0!(0)""")
          r1 <- runtime.getData(GInt(0L)).map(_.size)
          r2 <- runtime.getData(GInt(1L)).map(_.size)
          r3 <- runtime.getData(GInt(2L)).map(_.size)
          _  = r1 shouldBe 1
          _  = r2 shouldBe 1
        } yield (r3 shouldBe 2)
      }
      .runSyncUnsafe(2.seconds)
  }
}
