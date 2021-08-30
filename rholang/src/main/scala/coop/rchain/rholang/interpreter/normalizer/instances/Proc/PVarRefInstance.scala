package coop.rchain.rholang.interpreter.normalizer.instances.Proc
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Connective.ConnectiveInstance.VarRefBody
import coop.rchain.models.rholang.implicits.ParExtension
import coop.rchain.models.{Connective, Par, VarRef}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{PVarRef, VarRefKindName, VarRefKindProc}
import coop.rchain.rholang.interpreter.compiler.{
  IndexContext,
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
import coop.rchain.rholang.interpreter.normalizer.Normalizer
trait PVarRefInstance {
  implicit def PVarRefInstance[F[_]: Sync]
      : Normalizer[F, PVarRef, ProcVisitInputs, ProcVisitOutputs, Par] =
    new Normalizer[F, PVarRef, ProcVisitInputs, ProcVisitOutputs, Par] {
      override def normalize(p: PVarRef, input: ProcVisitInputs)(
          implicit env: Map[String, Par]
      ): F[ProcVisitOutputs] = input.env.find(p.var_) match {
        case None =>
          Sync[F].raiseError(UnboundVariableRef(p.var_, p.line_num, p.col_num))
        case Some((IndexContext(idx, kind, sourcePosition), depth)) =>
          kind match {
            case ProcSort =>
              p.varrefkind_ match {
                case _: VarRefKindProc =>
                  ProcVisitOutputs(
                    input.par
                      .prepend(Connective(VarRefBody(VarRef(idx, depth))), input.env.depth),
                    input.knownFree
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
                      .prepend(Connective(VarRefBody(VarRef(idx, depth))), input.env.depth),
                    input.knownFree
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
}
