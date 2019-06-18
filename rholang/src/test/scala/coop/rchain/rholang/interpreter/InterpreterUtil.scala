package coop.rchain.rholang.interpreter

import cats.effect.Sync
import coop.rchain.rholang.interpreter.accounting.{_cost, Cost}

object InterpreterUtil {
  def evaluate[F[_]: Sync: _cost](
      runtime: Runtime[F],
      term: String,
      initialPhlo: Cost
  ): F[EvaluateResult] = Interpreter[F].evaluate(runtime, term, initialPhlo, NormalizerEnv.Empty)
  def evaluate[F[_]: Sync: _cost](runtime: Runtime[F], term: String): F[EvaluateResult] =
    Interpreter[F].evaluate(runtime, term, NormalizerEnv.Empty)
}
