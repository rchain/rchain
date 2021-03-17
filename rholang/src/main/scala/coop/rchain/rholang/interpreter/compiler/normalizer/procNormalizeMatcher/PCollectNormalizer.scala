package coop.rchain.rholang.interpreter.compiler.normalizer

import cats.syntax.all._
import cats.effect.Sync
import coop.rchain.models.Par
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.compiler.{
  CollectVisitInputs,
  ProcVisitInputs,
  ProcVisitOutputs
}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.PCollect

object PCollectNormalizer {
  def normalize[F[_]: Sync](p: PCollect, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] =
    CollectionNormalizeMatcher
      .normalizeMatch[F](p.collection_, CollectVisitInputs(input.env, input.knownFree))
      .map(
        collectResult =>
          ProcVisitOutputs(
            input.par.prepend(collectResult.expr, input.env.depth),
            collectResult.knownFree
          )
      )
}
