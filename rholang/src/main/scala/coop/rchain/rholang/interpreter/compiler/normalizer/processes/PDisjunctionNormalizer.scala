package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Par
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.rholangN.Bindings._
import coop.rchain.models.rholangN._
import coop.rchain.rholang.ast.rholang_mercury.Absyn.PDisjunction
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler.{
  FreeMap,
  ProcVisitInputs,
  ProcVisitOutputs,
  SourcePosition
}

object PDisjunctionNormalizer {
  def normalize[F[_]: Sync](p: PDisjunction, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] =
    for {
      leftResult <- normalizeMatch[F](
                     p.proc_1,
                     ProcVisitInputs(VectorPar(), input.boundMapChain, FreeMap.empty)
                   )
      rightResult <- normalizeMatch[F](
                      p.proc_2,
                      ProcVisitInputs(VectorPar(), input.boundMapChain, FreeMap.empty)
                    )
      lp               = fromProto(leftResult.par)
      rp               = fromProto(rightResult.par)
      resultConnective = ConnOrN(Seq(lp, rp))

    } yield ProcVisitOutputs(
      toProto(fromProto(input.par).add(resultConnective)),
      input.freeMap
        .addConnective(
          toProtoConnective(resultConnective).connectiveInstance,
          SourcePosition(p.line_num, p.col_num)
        )
    )
}
