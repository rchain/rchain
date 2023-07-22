package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Par
import coop.rchain.models.rholangN.Bindings._
import coop.rchain.models.rholangN._
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
                     ProcVisitInputs(toProto(NilN()), input.boundMapChain, input.freeMap)
                   )
      rightResult <- normalizeMatch[F](
                      p.proc_2,
                      ProcVisitInputs(toProto(NilN()), input.boundMapChain, leftResult.freeMap)
                    )
      lp = fromProto(leftResult.par)
      rp = fromProto(rightResult.par)

      resultConnective = ConnAndN(Seq(lp, rp))

    } yield ProcVisitOutputs(
      toProto(fromProto(input.par).add(resultConnective)),
      rightResult.freeMap
        .addConnective(
          toProtoConnective(resultConnective).connectiveInstance,
          SourcePosition(p.line_num, p.col_num)
        )
    )
}
