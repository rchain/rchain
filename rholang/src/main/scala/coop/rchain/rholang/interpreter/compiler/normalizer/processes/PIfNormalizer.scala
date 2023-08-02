package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Par
import coop.rchain.models.rholangn._
import coop.rchain.rholang.ast.rholang_mercury.Absyn.Proc
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler.{ProcVisitInputs, ProcVisitOutputs}

object PIfNormalizer {
  def normalize[F[_]: Sync](
      valueProc: Proc,
      trueBodyProc: Proc,
      falseBodyProc: Proc,
      input: ProcVisitInputs
  )(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] =
    for {
      targetResult <- normalizeMatch[F](valueProc, input)
      trueCaseBody <- normalizeMatch[F](
                       trueBodyProc,
                       ProcVisitInputs(NilN(), input.boundMapChain, targetResult.freeMap)
                     )
      falseCaseBody <- normalizeMatch[F](
                        falseBodyProc,
                        ProcVisitInputs(NilN(), input.boundMapChain, trueCaseBody.freeMap)
                      )
      desugaredIf = MatchN(
        targetResult.par,
        Seq(
          MatchCaseN(GBoolN(true), trueCaseBody.par),
          MatchCaseN(GBoolN(false), falseCaseBody.par)
        )
      )
    } yield ProcVisitOutputs(input.par.combine(desugaredIf), falseCaseBody.freeMap)

}
