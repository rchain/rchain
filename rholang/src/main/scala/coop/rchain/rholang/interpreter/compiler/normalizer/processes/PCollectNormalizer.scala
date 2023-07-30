package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Par
import coop.rchain.models.rholangN.Bindings._
import coop.rchain.rholang.ast.rholang_mercury.Absyn.PCollect
import coop.rchain.rholang.interpreter.compiler.normalizer.CollectionNormalizeMatcher
import coop.rchain.rholang.interpreter.compiler.{
  CollectVisitInputs,
  ProcVisitInputs,
  ProcVisitOutputs
}

object PCollectNormalizer {
  def normalize[F[_]: Sync](p: PCollect, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] =
    CollectionNormalizeMatcher
      .normalizeMatch[F](p.collection_, CollectVisitInputs(input.boundMapChain, input.freeMap))
      .map {
        case collectResult =>
          val expr = collectResult.expr
          ProcVisitOutputs(input.par.combine(expr), collectResult.freeMap)
      }
}
