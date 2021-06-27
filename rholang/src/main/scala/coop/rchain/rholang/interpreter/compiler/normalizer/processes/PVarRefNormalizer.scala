package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.syntax.all._
import cats.effect.Sync
import coop.rchain.models.Connective.ConnectiveInstance.VarRefBody
import coop.rchain.models.{Connective, VarRef}
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.compiler.{
  BoundContext,
  NameSort,
  ProcSort,
  ProcVisitInputs,
  ProcVisitOutputs,
  SourcePosition
}
import coop.rchain.rholang.interpreter.errors.{
  UnboundVariableRef,
  UnexpectedNameContext,
  UnexpectedProcContext
}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{PVarRef, VarRefKindName, VarRefKindProc}

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
                ProcVisitOutputs(
                  input.par
                    .prepend(
                      Connective(VarRefBody(VarRef(idx, depth))),
                      input.boundMapChain.depth
                    ),
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
                ProcVisitOutputs(
                  input.par
                    .prepend(Connective(VarRefBody(VarRef(idx, depth))), input.boundMapChain.depth),
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
