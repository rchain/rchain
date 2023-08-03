package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Par
import coop.rchain.models.rholangn.Bindings._
import coop.rchain.models.rholangn._
import coop.rchain.rholang.ast.rholang_mercury.Absyn.PConjunction
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler.{ProcVisitInputs, ProcVisitOutputs, SourcePosition}

object PConjunctionNormalizer {
  def normalize[F[_]: Sync](p: PConjunction, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] =
    for {
      leftResult <- normalizeMatch[F](
                     p.proc_1,
                     ProcVisitInputs(NilN, input.boundMapChain, input.freeMap)
                   )
      rightResult <- normalizeMatch[F](
                      p.proc_2,
                      ProcVisitInputs(NilN, input.boundMapChain, leftResult.freeMap)
                    )
      lp = leftResult.par
      rp = rightResult.par

      resultConnective = ConnAndN(Seq(lp, rp))

    } yield ProcVisitOutputs(
      input.par.combine(resultConnective),
      rightResult.freeMap
        .addConnective(
          resultConnective,
          SourcePosition(p.line_num, p.col_num)
        )
    )
}
