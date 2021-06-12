package coop.rchain.rholang.interpreter.compiler.normalizer

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import coop.rchain.models.rholang.implicits.{VectorPar, _}
import coop.rchain.models.{EVar, Par, Var}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{Name, NameQuote, NameVar, NameWildcard}
import coop.rchain.rholang.interpreter.compiler.{
  IndexContext,
  LevelContext,
  NameSort,
  NameVisitInputs,
  NameVisitOutputs,
  ProcNormalizeMatcher,
  ProcSort,
  ProcVisitInputs,
  SourcePosition
}
import coop.rchain.rholang.interpreter.errors.{
  UnexpectedNameContext,
  UnexpectedReuseOfNameContextFree
}

object NameNormalizeMatcher {
  def normalizeMatch[F[_]: Sync](n: Name, input: NameVisitInputs)(
      implicit env: Map[String, Par]
  ): F[NameVisitOutputs] =
    n match {
      case wc: NameWildcard =>
        val wildcardBindResult =
          input.knownFree.addWildcard(SourcePosition(wc.line_num, wc.col_num))
        NameVisitOutputs(EVar(Wildcard(Var.WildcardMsg())), wildcardBindResult).pure[F]
      case n: NameVar =>
        input.env.get(n.var_) match {
          case Some(IndexContext(level, NameSort, _)) => {
            NameVisitOutputs(EVar(BoundVar(level)), input.knownFree).pure[F]
          }
          case Some(IndexContext(_, ProcSort, sourcePosition)) => {
            Sync[F].raiseError(
              UnexpectedNameContext(n.var_, sourcePosition, SourcePosition(n.line_num, n.col_num))
            )
          }
          case None => {
            input.knownFree.get(n.var_) match {
              case None =>
                val newBindingsPair =
                  input.knownFree.put((n.var_, NameSort, SourcePosition(n.line_num, n.col_num)))
                NameVisitOutputs(EVar(FreeVar(input.knownFree.nextLevel)), newBindingsPair).pure[F]
              case Some(LevelContext(_, _, sourcePosition)) =>
                Sync[F].raiseError(
                  UnexpectedReuseOfNameContextFree(
                    n.var_,
                    sourcePosition,
                    SourcePosition(n.line_num, n.col_num)
                  )
                )
            }
          }
        }

      case n: NameQuote => {
        ProcNormalizeMatcher
          .normalizeMatch[F](n.proc_, ProcVisitInputs(VectorPar(), input.env, input.knownFree))
          .map(
            procVisitResult => NameVisitOutputs(procVisitResult.par, procVisitResult.knownFree)
          )
      }
    }

}
