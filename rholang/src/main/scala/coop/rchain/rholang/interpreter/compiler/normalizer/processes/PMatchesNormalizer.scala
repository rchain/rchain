package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Par
import coop.rchain.models.rholangn._
import coop.rchain.rholang.ast.rholang_mercury.Absyn.PMatches
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler.{FreeMap, ProcVisitInputs, ProcVisitOutputs}

object PMatchesNormalizer {
  def normalize[F[_]: Sync](p: PMatches, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] =
    // In case of 'matches' expression the free variables from the pattern are thrown away
    // and only the ones from the target are used.
    // This is because the "target matches pattern" should have the same semantics as
    // "match target { pattern => true ; _ => false}
    // so free variables from pattern should not be visible at the top level
    for {
      leftResult <- normalizeMatch[F](p.proc_1, input.copy(par = NilN))
      rightResult <- normalizeMatch[F](
                      p.proc_2,
                      ProcVisitInputs(
                        NilN,
                        input.boundMapChain.push,
                        FreeMap.empty
                      )
                    )
    } yield ProcVisitOutputs(
      ParN.combine(input.par, EMatchesN(leftResult.par, rightResult.par)),
      leftResult.freeMap
    )
}
