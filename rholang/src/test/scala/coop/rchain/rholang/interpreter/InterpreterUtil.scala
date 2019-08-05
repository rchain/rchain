package coop.rchain.rholang.interpreter

import cats.implicits._
import cats.effect.Sync
import coop.rchain.metrics.Span
import coop.rchain.metrics.Span.TraceId
import coop.rchain.rholang.interpreter.accounting.{_cost, Cost}
import org.scalatest._
import org.scalatest.Matchers._

object InterpreterUtil {
  implicit val traceId: TraceId = Span.empty

  def evaluateResult[F[_]: Sync: _cost](
      runtime: Runtime[F],
      term: String,
      initialPhlo: Cost
  ): F[EvaluateResult] = Interpreter[F].evaluate(runtime, term, initialPhlo, NormalizerEnv.Empty)

  def evaluateResult[F[_]: Sync: _cost](runtime: Runtime[F], term: String): F[EvaluateResult] =
    Interpreter[F].evaluate(runtime, term, NormalizerEnv.Empty)

  def evaluate[F[_]: Sync: _cost](runtime: Runtime[F], term: String)(
      implicit line: sourcecode.Line,
      file: sourcecode.File
  ): F[Unit] =
    Interpreter[F].evaluate(runtime, term, NormalizerEnv.Empty).map {
      withClue(s"Evaluate was called at: ${file.value}:${line.value} and failed with: ") {
        _.errors shouldBe empty
      }
    }
}
