package coop.rchain.casper.helper

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import coop.rchain.casper.genesis.contracts.TestSetUtil
import coop.rchain.models.Expr.ExprInstance.{GBool, GString}
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.{Expr, ListParWithRandomAndPhlos, Par}
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.interpreter.Runtime.SystemProcess
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

object IsString {
  def unapply(p: Par): Option[String] =
    p.singleExpr().collect {
      case Expr(GString(bs)) => bs
    }
}

object IsBoolean {
  def unapply(p: Par): Option[Boolean] =
    p.singleExpr().collect {
      case Expr(GBool(b)) => b
    }
}

object IsCondition {
  def unapply(p: Seq[ListParWithRandomAndPhlos]): Option[(String, Boolean)] =
    p match {
      case Seq(ListParWithRandomAndPhlos(Seq(IsString(testName), IsBoolean(condition)), _, _)) =>
        Some((testName, condition))
      case _ => None
    }
}
object IsComparison {
  def unapply(p: Seq[ListParWithRandomAndPhlos]): Option[(String, Par, String, Par)] =
    p match {
      case Seq(
          ListParWithRandomAndPhlos(
            Seq(IsString(testName), expected, IsString(operator), actual),
            _,
            _
          )
          ) =>
        Some((testName, expected, operator, actual))
      case _ => None
    }
}

sealed trait RhoTestAssertion {
  val testName: String
}
case class RhoAssertTrue(testName: String, value: Boolean)               extends RhoTestAssertion
case class RhoAssertEquals(testName: String, expected: Any, actual: Any) extends RhoTestAssertion

private class TestResultCollector[F[_]: Sync](assertions: Ref[F, List[RhoTestAssertion]]) {
  def getAssertions: F[List[RhoTestAssertion]] = assertions.get

  def handleMessage(
      ctx: SystemProcess.Context[F]
  )(message: Seq[ListParWithRandomAndPhlos], x: Int): F[Unit] = {
    val assertion = message match {
      case IsComparison(testName, expected, "==", actual) =>
        RhoAssertEquals(testName, expected, actual)
      case IsCondition(testName, condition) => RhoAssertTrue(testName, condition)
    }
    assertions.update(assertion :: _)
  }
}

private object TestResultCollector {
  def apply[F[_]: Sync]: F[TestResultCollector[F]] =
    Ref
      .of(List.empty[RhoTestAssertion])
      .map(new TestResultCollector(_))
}

object RhoSpec {
  private def mkRuntime(testResultCollector: TestResultCollector[Task]) = {
    val testResultCollectorService =
      Seq((2, "assert"), (4, "assertEquals"))
        .zip(Stream.from(24))
        .map {
          case ((arity, name), n) =>
            SystemProcess.Definition[Task](
              s"rho:test:$name",
              Runtime.byteName(n.toByte),
              arity,
              n,
              testResultCollector.handleMessage
            )
        }
    TestSetUtil.runtime(testResultCollectorService)
  }

  def mkAssertions(testObject: CompiledRholangSource) =
    for {
      testResultCollector <- TestResultCollector[Task]

      _ <- Task.delay {
            TestSetUtil.runTests(testObject, List.empty, mkRuntime(testResultCollector))
          }

      assertions <- testResultCollector.getAssertions
    } yield assertions
}

class RhoSpec(testObject: CompiledRholangSource) extends FlatSpec with Matchers {
  def mkTest(assertion: RhoTestAssertion): Unit =
    it should assertion.testName in {
      assertion match {
        case RhoAssertEquals(testName, expected, actual) => actual should be(expected)
        case RhoAssertTrue(testName, value)              => value should be(true)
      }
    }

  RhoSpec
    .mkAssertions(testObject)
    .runSyncUnsafe(3.seconds)
    .foreach(mkTest)
}
