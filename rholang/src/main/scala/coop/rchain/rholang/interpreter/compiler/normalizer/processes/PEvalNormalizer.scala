package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.syntax.all._
import cats.effect.Sync
import coop.rchain.models.Par
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.compiler.{NameVisitInputs, ProcVisitInputs, ProcVisitOutputs}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.PEval
import coop.rchain.rholang.interpreter.compiler.normalizer.NameNormalizeMatcher

object PEvalNormalizer {
  def normalize[F[_]: Sync](p: PEval, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] =
    NameNormalizeMatcher
      .normalizeMatch[F](p.name_, NameVisitInputs(input.env, input.knownFree))
      .map(
        nameMatchResult =>
          ProcVisitOutputs(
            input.par ++ nameMatchResult.chan,
            nameMatchResult.knownFree
          )
      )
}
