package coop.rchain.casper.helper
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Expr.ExprInstance.{ETupleBody, GBool, GString}
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.{ETuple, Expr, ListParWithRandomAndPhlos, Par, _}
import coop.rchain.rholang.interpreter.Runtime.SystemProcess
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.storage.implicits.matchListPar
import coop.rchain.rspace.util.unpackCont

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

object IsAssert {
  def unapply(
      p: Seq[ListParWithRandomAndPhlos]
  ): Option[(String, Par, String, AckedActionCtx)] =
    p match {
      case Seq(
          ListParWithRandomAndPhlos(
            Seq(IsString(testName), assertion, IsString(clue), ackChannel),
            rand,
            sequenceNumber
          )
          ) =>
        Some((testName, assertion, clue, AckedActionCtx(ackChannel, rand, sequenceNumber)))
      case _ => None
    }
}

object IsComparison {
  def unapply(
      p: Par
  ): Option[(Par, String, Par)] =
    p.singleExpr().collect {
      case Expr(ETupleBody(ETuple(List(expected, IsString(operator), actual), _, _))) =>
        (expected, operator, actual)
    }
}
object IsSetFinished {
  def unapply(p: Seq[ListParWithRandomAndPhlos]): Option[Boolean] =
    p match {
      case Seq(
          ListParWithRandomAndPhlos(
            Seq(IsBoolean(hasFinished)),
            _,
            _
          )
          ) =>
        Some(hasFinished)
      case _ => None
    }
}

sealed trait RhoTestAssertion {
  val testName: String
  val clue: String
}

case class RhoAssertTrue(testName: String, value: Boolean, clue: String) extends RhoTestAssertion
case class RhoAssertEquals(testName: String, expected: Any, actual: Any, clue: String)
    extends RhoTestAssertion

case class TestResult(assertions: Map[String, List[RhoTestAssertion]], hasFinished: Boolean) {
  def addAssertion(assertion: RhoTestAssertion): TestResult = {
    val newAssertion =
      (assertion.testName, assertion :: assertions.getOrElse(assertion.testName, List.empty))
    TestResult(assertions + newAssertion, hasFinished)
  }
  def setFinished(hasFinished: Boolean): TestResult =
    TestResult(assertions, hasFinished = hasFinished)
}

case class AckedActionCtx(ackChannel: Par, rand: Blake2b512Random, sequenceNumber: Long)

object TestResultCollector {
  def apply[F[_]: Sync]: F[TestResultCollector[F]] =
    Ref
      .of(TestResult(Map.empty, hasFinished = false))
      .map(new TestResultCollector(_))
}

class TestResultCollector[F[_]: Sync](result: Ref[F, TestResult]) {

  def getResult: F[TestResult] = result.get

  private def runWithAck[T](
      ctx: SystemProcess.Context[F],
      actionCtx: AckedActionCtx,
      action: F[T],
      ackValue: Par
  ): F[T] = {
    val UNLIMITED_MATCH_PHLO = matchListPar(Cost(Integer.MAX_VALUE))

    def sendAck(ackValue: Par) =
      for {
        produceResult <- ctx.space.produce(
                          actionCtx.ackChannel,
                          ListParWithRandom(Seq(ackValue), actionCtx.rand),
                          persist = false,
                          actionCtx.sequenceNumber.toInt
                        )(UNLIMITED_MATCH_PHLO)

        _ <- produceResult.fold(
              _ => Sync[F].raiseError(OutOfPhlogistonsError),
              _.fold(Sync[F].unit) {
                case (cont, channels) =>
                  ctx.dispatcher
                    .dispatch(unpackCont(cont), channels.map(_.value), cont.sequenceNumber)
              }
            )
      } yield ()

    action <* sendAck(ackValue)
  }

  def handleMessage(
      ctx: SystemProcess.Context[F]
  )(message: Seq[ListParWithRandomAndPhlos], x: Int): F[Unit] =
    message match {
      case IsAssert(testName, assertion, clue, ackedActionCtx) =>
        assertion match {
          case IsComparison(expected, "==", actual) =>
            runWithAck(
              ctx,
              ackedActionCtx,
              result.update(_.addAssertion(RhoAssertEquals(testName, expected, actual, clue))),
              Expr(GBool(expected == actual))
            )
          case IsBoolean(condition) =>
            runWithAck(
              ctx,
              ackedActionCtx,
              result.update(_.addAssertion(RhoAssertTrue(testName, condition, clue))),
              Expr(GBool(condition))
            )

          case _ =>
            runWithAck(
              ctx,
              ackedActionCtx,
              result.update(
                _.addAssertion(
                  RhoAssertTrue(
                    testName,
                    value = false,
                    s"Failed to evaluate assertion $assertion"
                  )
                )
              ),
              Expr(GBool(false))
            )
        }
      case IsSetFinished(hasFinished) => result.update(_.setFinished(hasFinished))
    }
}
