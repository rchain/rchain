package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Par
import coop.rchain.rholang.ast.rholang_mercury.Absyn.PPar
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler.{ProcVisitInputs, ProcVisitOutputs}

object PParNormalizer {
  def normalize[F[_]: Sync](p: PPar, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] =
    Sync[F].defer {
      for {
        result       <- normalizeMatch[F](p.proc_1, input)
        chainedInput = input.copy(freeMap = result.freeMap, par = result.par)
        chainedRes   <- normalizeMatch[F](p.proc_2, chainedInput)
      } yield chainedRes
    }
}
