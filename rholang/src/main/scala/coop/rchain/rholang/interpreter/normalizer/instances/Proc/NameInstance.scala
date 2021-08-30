package coop.rchain.rholang.interpreter.normalizer.instances.Proc

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.{EVar, Par, Var}
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{Name, NameQuote, NameVar, NameWildcard, Proc}
import coop.rchain.rholang.interpreter.compiler.{
  IndexContext,
  LevelContext,
  NameSort,
  ProcSort,
  SourcePosition
}
import coop.rchain.rholang.interpreter.errors.{
  UnexpectedNameContext,
  UnexpectedReuseOfNameContextFree
}
import coop.rchain.rholang.interpreter.normalizer.Normalizer
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.compiler.Visit._

trait NameInstance {
  implicit def nameInstance[F[_]: Sync]
      : Normalizer[F, Name, NameVisitInputs, NameVisitOutputs, Par] =
    new Normalizer[F, Name, NameVisitInputs, NameVisitOutputs, Par] {
      override def normalize(p: Name, input: NameVisitInputs[Par])(
          implicit env: Map[String, Par]
      ): F[NameVisitOutputs[Par]] = p match {
        case wc: NameWildcard =>
          val wildcardBindResult =
            input.knownFree.addWildcard(SourcePosition(wc.line_num, wc.col_num))
          NameVisitOutputs(EVar(Wildcard(Var.WildcardMsg())): Par, wildcardBindResult).pure[F]
        case n: NameVar =>
          input.env.get(n.var_) match {
            case Some(IndexContext(level, NameSort, _)) => {
              NameVisitOutputs(EVar(BoundVar(level)): Par, input.knownFree).pure[F]
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
                  NameVisitOutputs(EVar(FreeVar(input.knownFree.nextLevel)): Par, newBindingsPair)
                    .pure[F]
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
          Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par]
            .normalize(n.proc_, ProcVisitInputs(VectorPar(), input.env, input.knownFree))
            .map(
              procVisitResult => NameVisitOutputs(procVisitResult.par, procVisitResult.knownFree)
            )
        }
      }
    }
}
