package coop.rchain.rholang.interpreter.compiler.normalizer

import cats.syntax.all._
import cats.effect.Sync
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler.{ProcVisitInputs, ProcVisitOutputs}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.PPar

object PParNormalizer {
  def normalize[F[_]: Sync](p: PPar, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] =
    Sync[F].suspend {
      for {
        result       <- normalizeMatch[F](p.proc_1, input)
        chainedInput = input.copy(knownFree = result.knownFree, par = result.par)
        chainedRes   <- normalizeMatch[F](p.proc_2, chainedInput)
      } yield chainedRes
    }
}
