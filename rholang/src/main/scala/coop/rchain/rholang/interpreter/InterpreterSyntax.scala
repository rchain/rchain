package coop.rchain.rholang.interpreter

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par

trait InterpreterSyntax {
  implicit final def syntaxInterpreter[F[_]: Sync](
      interpreter: Interpreter[F]
  ): InterpreterOps[F] =
    new InterpreterOps[F](interpreter)
}

final class InterpreterOps[F[_]: Sync](
    private val interpreter: Interpreter[F]
) {

  def evaluate(
      runtime: Runtime[F],
      term: String,
      normalizerEnv: Map[String, Par]
  ): F[EvaluateResult] =
    evaluate(runtime, term, Cost.UNSAFE_MAX, normalizerEnv)

  def evaluate(
      runtime: Runtime[F],
      term: String
  ): F[EvaluateResult] =
    evaluate(runtime, term, Cost.UNSAFE_MAX, Map.empty)

  def evaluate(
      runtime: Runtime[F],
      term: String,
      initialPhlo: Cost
  ): F[EvaluateResult] = evaluate(runtime, term, initialPhlo, Map.empty)

  def evaluate(
      runtime: Runtime[F],
      term: String,
      initialPhlo: Cost,
      normalizerEnv: Map[String, Par]
  ): F[EvaluateResult] = {
    implicit val rand: Blake2b512Random = Blake2b512Random(128)
    runtime.space.createSoftCheckpoint() >>= { checkpoint =>
      interpreter.injAttempt(runtime.reducer, term, initialPhlo, normalizerEnv).attempt >>= {
        case Right(evaluateResult) =>
          if (evaluateResult.errors.nonEmpty)
            runtime.space.revertToSoftCheckpoint(checkpoint).as(evaluateResult)
          else evaluateResult.pure[F]
        case Left(throwable) =>
          runtime.space.revertToSoftCheckpoint(checkpoint) >> throwable
            .raiseError[F, EvaluateResult]
      }
    }
  }
}
