package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.rholangN._
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{PVar, ProcVarVar, ProcVarWildcard}
import coop.rchain.rholang.interpreter.compiler._
import coop.rchain.rholang.interpreter.errors.{
  UnexpectedProcContext,
  UnexpectedReuseOfProcContextFree
}

object PVarNormalizer {
  def normalize[F[_]: Sync](p: PVar, input: ProcVisitInputs): F[ProcVisitOutputs] =
    p.procvar_ match {
      case pvv: ProcVarVar =>
        input.boundMapChain.get(pvv.var_) match {
          case Some(BoundContext(level, ProcSort, _)) =>
            ProcVisitOutputs(
              input.par.combine(BoundVarN(level)),
              input.freeMap
            ).pure[F]
          case Some(BoundContext(_, NameSort, sourcePosition)) =>
            Sync[F].raiseError(
              UnexpectedProcContext(
                pvv.var_,
                sourcePosition,
                SourcePosition(pvv.line_num, pvv.col_num)
              )
            )
          case None =>
            input.freeMap.get(pvv.var_) match {
              case None =>
                val newBindingsPair =
                  input.freeMap.put(
                    (pvv.var_, ProcSort, SourcePosition(pvv.line_num, pvv.col_num))
                  )
                ProcVisitOutputs(
                  input.par.combine(FreeVarN(input.freeMap.nextLevel)),
                  newBindingsPair
                ).pure[F]
              case Some(FreeContext(_, _, firstSourcePosition)) =>
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
          input.par.combine(WildcardN()),
          input.freeMap.addWildcard(SourcePosition(p.line_num, p.col_num))
        ).pure[F]
    }
}
