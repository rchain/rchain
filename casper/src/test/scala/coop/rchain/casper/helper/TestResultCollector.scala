package coop.rchain.casper.helper
import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.Span
import coop.rchain.models.Expr.ExprInstance.{ETupleBody, GBool}
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.{ETuple, Expr, ListParWithRandom, Par}
import coop.rchain.rholang.interpreter.SystemProcesses.ProcessContext
import coop.rchain.rholang.interpreter.{ContractCall, RhoType}

object IsAssert {
  def unapply(
      p: Seq[Par]
  ): Option[(Par, String, Long, Par, String)] =
    p match {
      case Seq(
          ackChannel,
          RhoType.String(testName),
          RhoType.Number(attempt),
          assertion,
          RhoType.String(clue)
          ) =>
        Some((ackChannel, testName, attempt, assertion, clue))
      case _ => None
    }
}

object IsComparison {
  def unapply(
      p: Par
  ): Option[(Par, String, Par)] =
    p.singleExpr().collect {
      case Expr(ETupleBody(ETuple(List(expected, RhoType.String(operator), actual), _, _))) =>
        (expected, operator, actual)
    }
}
object IsSetFinished {
  def unapply(p: Seq[Par]): Option[Boolean] =
    p match {
      case Seq(RhoType.Boolean(hasFinished)) =>
        Some(hasFinished)
      case _ => None
    }
}

sealed trait RhoTestAssertion {
  val testName: String
  val clue: String
  val isSuccess: Boolean
}

case class RhoAssertTrue(testName: String, override val isSuccess: Boolean, clue: String)
    extends RhoTestAssertion
case class RhoAssertEquals(testName: String, expected: Par, actual: Par, clue: String)
    extends RhoTestAssertion {
  override val isSuccess: Boolean = actual == expected
}
case class RhoAssertNotEquals(testName: String, unexpected: Par, actual: Par, clue: String)
    extends RhoTestAssertion {
  override val isSuccess: Boolean = actual != unexpected
}

case class TestResult(
    assertions: Map[String, Map[Long, List[RhoTestAssertion]]],
    hasFinished: Boolean
) {
  def addAssertion(attempt: Long, assertion: RhoTestAssertion): TestResult = {
    val currentAttemptAssertions = assertions.getOrElse(assertion.testName, Map.empty)
    val newAssertion =
      (attempt, assertion :: currentAttemptAssertions.getOrElse(attempt, List.empty))
    val newCurrentAttemptAssertions = currentAttemptAssertions + newAssertion
    TestResult(assertions.updated(assertion.testName, newCurrentAttemptAssertions), hasFinished)
  }
  def setFinished(hasFinished: Boolean): TestResult =
    TestResult(assertions, hasFinished = hasFinished)
}

case class AckedActionCtx(ackChannel: Par, rand: Blake2b512Random, sequenceNumber: Long)

object TestResultCollector {
  def apply[F[_]: Concurrent: Span]: F[TestResultCollector[F]] =
    Ref
      .of(TestResult(Map.empty, hasFinished = false))
      .map(new TestResultCollector(_))
}

class TestResultCollector[F[_]: Concurrent: Span](result: Ref[F, TestResult]) {

  def getResult: F[TestResult] = result.get

  def handleMessage(
      ctx: ProcessContext[F]
  )(message: Seq[ListParWithRandom]): F[Unit] = {

    val isContractCall = new ContractCall[F](ctx.space, ctx.dispatcher)

    message match {
      case isContractCall(produce, IsAssert(ackChannel, testName, attempt, assertion, clue)) =>
        assertion match {
          case IsComparison(expected, "==", actual) =>
            val assertion = RhoAssertEquals(testName, expected, actual, clue)
            for {
              _ <- result.update(_.addAssertion(attempt, assertion))
              _ <- produce(Seq(Expr(GBool(assertion.isSuccess))), ackChannel)
            } yield ()
          case IsComparison(unexpected, "!=", actual) =>
            val assertion = RhoAssertNotEquals(testName, unexpected, actual, clue)
            for {
              _ <- result.update(_.addAssertion(attempt, assertion))
              _ <- produce(Seq(Expr(GBool(assertion.isSuccess))), ackChannel)
            } yield ()
          case RhoType.Boolean(condition) =>
            for {
              _ <- result.update(_.addAssertion(attempt, RhoAssertTrue(testName, condition, clue)))
              _ <- produce(Seq(Expr(GBool(condition))), ackChannel)
            } yield ()

          case _ =>
            for {
              _ <- result.update(
                    _.addAssertion(
                      attempt,
                      RhoAssertTrue(
                        testName,
                        isSuccess = false,
                        s"Failed to evaluate assertion $assertion"
                      )
                    )
                  )
              _ <- produce(Seq(Expr(GBool(false))), ackChannel)
            } yield ()
        }
      case isContractCall(_, IsSetFinished(hasFinished)) =>
        result.update(_.setFinished(hasFinished))
    }
  }
}
