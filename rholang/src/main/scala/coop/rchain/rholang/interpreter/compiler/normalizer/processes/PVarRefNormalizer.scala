package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.rholangN.Bindings._
import coop.rchain.models.rholangN._
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{PVarRef, VarRefKindName, VarRefKindProc}
import coop.rchain.rholang.interpreter.compiler._
import coop.rchain.rholang.interpreter.errors.{
  UnboundVariableRef,
  UnexpectedNameContext,
  UnexpectedProcContext
}

object PVarRefNormalizer {
  def normalize[F[_]: Sync](p: PVarRef, input: ProcVisitInputs): F[ProcVisitOutputs] =
    input.boundMapChain.find(p.var_) match {
      case None =>
        Sync[F].raiseError(UnboundVariableRef(p.var_, p.line_num, p.col_num))
      case Some((BoundContext(idx, kind, sourcePosition), depth)) =>
        kind match {
          case ProcSort =>
            p.varrefkind_ match {
              case _: VarRefKindProc =>
                val inpPar = fromProto(input.par)
                ProcVisitOutputs(
                  toProto(inpPar.add(ConnVarRefN(idx, depth))),
                  input.freeMap
                ).pure[F]
              case _ =>
                Sync[F].raiseError(
                  UnexpectedProcContext(
                    p.var_,
                    sourcePosition,
                    SourcePosition(p.line_num, p.col_num)
                  )
                )
            }
          case NameSort =>
            p.varrefkind_ match {
              case _: VarRefKindName =>
                val inpPar = fromProto(input.par)
                ProcVisitOutputs(
                  toProto(inpPar.add(ConnVarRefN(idx, depth))),
                  input.freeMap
                ).pure[F]
              case _ =>
                Sync[F].raiseError(
                  UnexpectedNameContext(
                    p.var_,
                    sourcePosition,
                    SourcePosition(p.line_num, p.col_num)
                  )
                )
            }
        }
    }
}
