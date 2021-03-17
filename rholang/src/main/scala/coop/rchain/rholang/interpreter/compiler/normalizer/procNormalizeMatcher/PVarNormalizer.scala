package coop.rchain.rholang.interpreter.compiler.normalizer

import cats.syntax.all._
import cats.effect.Sync
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import coop.rchain.models.{EVar, Par, Var}
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.compiler.{
  IndexContext,
  LevelContext,
  NameSort,
  ProcSort,
  ProcVisitInputs,
  ProcVisitOutputs,
  SourcePosition
}
import coop.rchain.rholang.interpreter.errors.{
  UnexpectedProcContext,
  UnexpectedReuseOfProcContextFree
}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{PVar, ProcVarVar, ProcVarWildcard}

object PVarNormalizer {
  def normalize[F[_]: Sync](p: PVar, input: ProcVisitInputs): F[ProcVisitOutputs] =
    p.procvar_ match {
      case pvv: ProcVarVar =>
        input.env.get(pvv.var_) match {
          case Some(IndexContext(level, ProcSort, _)) =>
            ProcVisitOutputs(
              input.par.prepend(EVar(BoundVar(level)), input.env.depth),
              input.knownFree
            ).pure[F]
          case Some(IndexContext(_, NameSort, sourcePosition)) =>
            Sync[F].raiseError(
              UnexpectedProcContext(
                pvv.var_,
                sourcePosition,
                SourcePosition(pvv.line_num, pvv.col_num)
              )
            )
          case None =>
            input.knownFree.get(pvv.var_) match {
              case None =>
                val newBindingsPair =
                  input.knownFree.put(
                    (pvv.var_, ProcSort, SourcePosition(pvv.line_num, pvv.col_num))
                  )
                ProcVisitOutputs(
                  input.par
                    .prepend(EVar(FreeVar(input.knownFree.nextLevel)), input.env.depth)
                    .withConnectiveUsed(true),
                  newBindingsPair
                ).pure[F]
              case Some(LevelContext(_, _, firstSourcePosition)) =>
                Sync[F].raiseError(
                  UnexpectedReuseOfProcContextFree(
                    pvv.var_,
                    firstSourcePosition,
                    SourcePosition(pvv.line_num, pvv.col_num)
                  )
                )
            }
        }
      case _: ProcVarWildcard =>
        ProcVisitOutputs(
          input.par
            .prepend(EVar(Wildcard(Var.WildcardMsg())), input.env.depth)
            .withConnectiveUsed(true),
          input.knownFree.addWildcard(SourcePosition(p.line_num, p.col_num))
        ).pure[F]
    }
}
