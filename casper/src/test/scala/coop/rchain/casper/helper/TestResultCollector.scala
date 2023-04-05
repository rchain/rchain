package coop.rchain.casper.helper
import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.Span
import coop.rchain.models.Expr.ExprInstance.{ETupleBody, GBool}
import coop.rchain.models.rholang.RhoType
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.{ETuple, Expr, ListParWithRandom, Par}
import coop.rchain.rholang.interpreter.ContractCall
import coop.rchain.rholang.interpreter.SystemProcesses.ProcessContext
import cats.effect.Ref

object IsAssert {
  def unapply(
      p: Seq[Par]
  ): Option[(String, Long, Par, String, Par)] =
    p match {
      case Seq(
          RhoType.RhoString(testName),
          RhoType.RhoNumber(attempt),
          assertion,
          RhoType.RhoString(clue),
          ackChannel
          ) =>
        Some((testName, attempt, assertion, clue, ackChannel))
      case _ => None
    }
}

object IsComparison {
  def unapply(
      p: Par
  ): Option[(Par, String, Par)] =
    p.singleExpr().collect {
      case Expr(ETupleBody(ETuple(List(expected, RhoType.RhoString(operator), actual), _, _))) =>
        (expected, operator, actual)
    }
}
object IsSetFinished {
  def unapply(p: Seq[Par]): Option[Boolean] =
    p match {
      case Seq(RhoType.RhoBoolean(hasFinished)) =>
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
      case isContractCall(produce, IsAssert(testName, attempt, assertion, clue, ackChannel)) =>
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
          case RhoType.RhoBoolean(condition) =>
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
