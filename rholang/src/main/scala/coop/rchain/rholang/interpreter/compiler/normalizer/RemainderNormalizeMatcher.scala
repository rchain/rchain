package coop.rchain.rholang.interpreter.compiler.normalizer

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Var
import coop.rchain.models.Var.VarInstance.{FreeVar, Wildcard}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{
  NameRemainder,
  NameRemainderEmpty,
  NameRemainderVar,
  ProcRemainder,
  ProcRemainderEmpty,
  ProcRemainderVar,
  ProcVar,
  ProcVarVar,
  ProcVarWildcard
}
import coop.rchain.rholang.interpreter.compiler.{
  DeBruijnLevelMap,
  LevelContext,
  ProcSort,
  SourcePosition,
  VarSort
}
import coop.rchain.rholang.interpreter.errors.UnexpectedReuseOfProcContextFree

object RemainderNormalizeMatcher {
  def handleProcVar[F[_]: Sync](
      pv: ProcVar,
      knownFree: DeBruijnLevelMap[VarSort]
  ): F[(Option[Var], DeBruijnLevelMap[VarSort])] =
    pv match {
      case pvw: ProcVarWildcard =>
        (
          Option(Var(Wildcard(Var.WildcardMsg()))),
          knownFree.addWildcard(SourcePosition(pvw.line_num, pvw.col_num))
        ).pure[F]
      case pvv: ProcVarVar =>
        val sourcePosition = SourcePosition(pvv.line_num, pvv.col_num)
        knownFree.get(pvv.var_) match {
          case None =>
            val newBindingsPair = knownFree.put((pvv.var_, ProcSort, sourcePosition))
            (Option(Var(FreeVar(knownFree.nextLevel))), newBindingsPair).pure[F]
          case Some(LevelContext(_, _, firstSourcePosition)) =>
            Sync[F].raiseError(
              UnexpectedReuseOfProcContextFree(pvv.var_, firstSourcePosition, sourcePosition)
            )
        }
    }

  def normalizeMatchProc[F[_]: Sync](
      r: ProcRemainder,
      knownFree: DeBruijnLevelMap[VarSort]
  ): F[(Option[Var], DeBruijnLevelMap[VarSort])] =
    r match {
      case _: ProcRemainderEmpty => (None: Option[Var], knownFree).pure[F]
      case pr: ProcRemainderVar =>
        handleProcVar[F](pr.procvar_, knownFree)
    }

  def normalizeMatchName[F[_]: Sync](
      nr: NameRemainder,
      knownFree: DeBruijnLevelMap[VarSort]
  ): F[(Option[Var], DeBruijnLevelMap[VarSort])] =
    nr match {
      case _: NameRemainderEmpty => (None: Option[Var], knownFree).pure[F]
      case nr: NameRemainderVar =>
        handleProcVar[F](nr.procvar_, knownFree)
    }
}
