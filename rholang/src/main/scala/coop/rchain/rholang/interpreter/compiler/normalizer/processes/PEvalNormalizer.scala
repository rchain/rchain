package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Par
import coop.rchain.models.rholangn.ParN
import coop.rchain.rholang.ast.rholang_mercury.Absyn.PEval
import coop.rchain.rholang.interpreter.compiler.normalizer.NameNormalizeMatcher
import coop.rchain.rholang.interpreter.compiler.{NameVisitInputs, ProcVisitInputs, ProcVisitOutputs}

object PEvalNormalizer {
  def normalize[F[_]: Sync](p: PEval, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] =
    NameNormalizeMatcher
      .normalizeMatch[F](p.name_, NameVisitInputs(input.boundMapChain, input.freeMap))
      .map(
        nameMatchResult =>
          ProcVisitOutputs(
            ParN.combine(input.par, nameMatchResult.par),
            nameMatchResult.freeMap
          )
      )
}
