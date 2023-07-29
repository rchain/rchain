package coop.rchain.rholang.interpreter.compiler.normalizer

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Par
import coop.rchain.models.rholangN.Bindings._
import coop.rchain.models.rholangN._
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{Name, NameQuote, NameVar, NameWildcard}
import coop.rchain.rholang.interpreter.compiler._
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
          input.freeMap.addWildcard(SourcePosition(wc.line_num, wc.col_num))
        NameVisitOutputs(toProto(WildcardN()), wildcardBindResult).pure[F]
      case n: NameVar =>
        input.boundMapChain.get(n.var_) match {
          case Some(BoundContext(level, NameSort, _)) => {
            NameVisitOutputs(toProto(BoundVarN(level)), input.freeMap).pure[F]
          }
          case Some(BoundContext(_, ProcSort, sourcePosition)) => {
            Sync[F].raiseError(
              UnexpectedNameContext(n.var_, sourcePosition, SourcePosition(n.line_num, n.col_num))
            )
          }
          case None => {
            input.freeMap.get(n.var_) match {
              case None =>
                val newBindingsPair =
                  input.freeMap.put((n.var_, NameSort, SourcePosition(n.line_num, n.col_num)))
                NameVisitOutputs(toProto(FreeVarN(input.freeMap.nextLevel)), newBindingsPair)
                  .pure[F]
              case Some(FreeContext(_, _, sourcePosition)) =>
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

      case n: NameQuote =>
        ProcNormalizeMatcher
          .normalizeMatch[F](n.proc_, ProcVisitInputs(NilN(), input.boundMapChain, input.freeMap))
          .map(
            procVisitResult =>
              NameVisitOutputs(toProto(procVisitResult.par), procVisitResult.freeMap)
          )

    }

}
