package coop.rchain.rholang.interpreter.compiler.normalizer

import cats.syntax.all._
import cats.effect.Sync
import coop.rchain.models.Connective.ConnectiveInstance.ConnNotBody
import coop.rchain.models.{Connective, Par}
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler.{
  DeBruijnLevelMap,
  ProcVisitInputs,
  ProcVisitOutputs,
  SourcePosition
}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.PNegation

object PNegationNormalizer {
  def normalize[F[_]: Sync](p: PNegation, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] =
    normalizeMatch[F](
      p.proc_,
      ProcVisitInputs(VectorPar(), input.env, DeBruijnLevelMap.empty)
    ).map(
      bodyResult =>
        ProcVisitOutputs(
          input.par.prepend(Connective(ConnNotBody(bodyResult.par)), input.env.depth),
          input.knownFree
            .addConnective(
              ConnNotBody(bodyResult.par),
              SourcePosition(p.line_num, p.col_num)
            )
        )
    )
}
