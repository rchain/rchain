package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Par
import coop.rchain.models.rholangN.Bindings._
import coop.rchain.models.rholangN._
import coop.rchain.rholang.ast.rholang_mercury.Absyn.PNegation
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler.{
  FreeMap,
  ProcVisitInputs,
  ProcVisitOutputs,
  SourcePosition
}

object PNegationNormalizer {
  def normalize[F[_]: Sync](p: PNegation, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] =
    normalizeMatch[F](
      p.proc_,
      ProcVisitInputs(NilN(), input.boundMapChain, FreeMap.empty)
    ).map { bodyResult =>
      val conn = ConnNotN(bodyResult.par)
      ProcVisitOutputs(
        input.par.add(conn),
        input.freeMap.addConnective(
          toProtoConnective(conn).connectiveInstance,
          SourcePosition(p.line_num, p.col_num)
        )
      )
    }
}
