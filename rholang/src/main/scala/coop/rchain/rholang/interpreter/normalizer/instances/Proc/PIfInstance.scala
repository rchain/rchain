package coop.rchain.rholang.interpreter.normalizer.instances.Proc

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Expr.ExprInstance.GBool
import coop.rchain.models.rholang.implicits.VectorPar
import coop.rchain.models.{Match, MatchCase, Par}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{PIf, PIfElse, PNil, Proc}
import coop.rchain.rholang.interpreter.compiler.Visit._
import coop.rchain.rholang.interpreter.normalizer.Normalizer
import coop.rchain.models.rholang.implicits._

import scala.collection.immutable.Vector
trait PIfInstance {
  implicit def PIfInstance[F[_]: Sync]: Normalizer[F, PIf, ProcVisitInputs, ProcVisitOutputs, Par] =
    new Normalizer[F, PIf, ProcVisitInputs, ProcVisitOutputs, Par] {
      override def normalize(p: PIf, input: ProcVisitInputs)(
          implicit env: Map[String, Par]
      ): F[ProcVisitOutputs] =
        normalizeIF(p.proc_1, p.proc_2, new PNil(), input.copy(par = VectorPar()))
          .map(n => n.copy(par = n.par ++ input.par))
    }

  implicit def PIfElseInstance[F[_]: Sync]
      : Normalizer[F, PIfElse, ProcVisitInputs, ProcVisitOutputs, Par] =
    new Normalizer[F, PIfElse, ProcVisitInputs, ProcVisitOutputs, Par] {
      override def normalize(p: PIfElse, input: ProcVisitInputs)(
          implicit env: Map[String, Par]
      ): F[ProcVisitOutputs] =
        normalizeIF(p.proc_1, p.proc_2, p.proc_3, input.copy(par = VectorPar()))
          .map(n => n.copy(par = n.par ++ input.par))
    }

  private def normalizeIF[F[_]: Sync](
      valueProc: Proc,
      trueBodyProc: Proc,
      falseBodyProc: Proc,
      input: ProcVisitInputs
  )(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] =
    for {
      targetResult <- Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par]
                       .normalize(valueProc, input)
      trueCaseBody <- Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par].normalize(
                       trueBodyProc,
                       ProcVisitInputs(VectorPar(), input.env, targetResult.knownFree)
                     )
      falseCaseBody <- Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par].normalize(
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
