package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.syntax.all._
import cats.effect.Sync
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.compiler.{ProcVisitInputs, ProcVisitOutputs}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.PGround
import coop.rchain.rholang.interpreter.compiler.normalizer.GroundNormalizeMatcher

object PGroundNormalizer {
  def normalize[F[_]: Sync](p: PGround, input: ProcVisitInputs): F[ProcVisitOutputs] =
    GroundNormalizeMatcher
      .normalizeMatch[F](p.ground_)
      .map(
        expr =>
          ProcVisitOutputs(
            input.par.prepend(expr, input.env.depth),
            input.knownFree
          )
      )
}
