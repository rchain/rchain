package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.syntax.all._
import cats.effect.Sync
import coop.rchain.models.Expr.ExprInstance.GBool
import coop.rchain.models.{Match, MatchCase, Par}
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler.{ProcVisitInputs, ProcVisitOutputs}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.Proc

import scala.collection.immutable.Vector

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
                       ProcVisitInputs(VectorPar(), input.env, targetResult.knownFree)
                     )
      falseCaseBody <- normalizeMatch[F](
                        falseBodyProc,
                        ProcVisitInputs(VectorPar(), input.env, trueCaseBody.knownFree)
                      )
      desugaredIf = Match(
        targetResult.par,
        Vector(
          MatchCase(GBool(true), trueCaseBody.par, 0),
          MatchCase(GBool(false), falseCaseBody.par, 0)
        ),
        targetResult.par.locallyFree | trueCaseBody.par.locallyFree | falseCaseBody.par.locallyFree,
        targetResult.par.connectiveUsed || trueCaseBody.par.connectiveUsed || falseCaseBody.par.connectiveUsed
      )
    } yield ProcVisitOutputs(input.par.prepend(desugaredIf), falseCaseBody.knownFree)

}
