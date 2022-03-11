package coop.rchain.rholang.interpreter.compiler.normalizer.processes

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
import coop.rchain.rholang.interpreter.compiler.normalizer.CollectionNormalizeMatcher

object PCollectNormalizer {
  def normalize[F[_]: Sync](p: PCollect, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] =
    CollectionNormalizeMatcher
      .normalizeMatch[F](p.collection_, CollectVisitInputs(input.boundMapChain, input.freeMap))
      .map(
        collectResult =>
          ProcVisitOutputs(
            input.par.prepend(collectResult.expr, input.boundMapChain.depth),
            collectResult.freeMap
          )
      )
}
