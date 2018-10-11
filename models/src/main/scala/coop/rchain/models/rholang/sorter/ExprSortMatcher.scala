package coop.rchain.models.rholang.sorter

import cats.effect.Sync
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import cats.implicits._

private[sorter] object ExprSortMatcher extends Sortable[Expr] {

  def sortMatch[F[_]: Sync](e: Expr): F[ScoredTerm[Expr]] = {
    def constructExpr(exprInstance: ExprInstance, score: Tree[ScoreAtom]) =
      ScoredTerm(Expr(exprInstance = exprInstance), score)

    e.exprInstance match {
      case ENegBody(en) =>
        for {
          sortedPar <- Sortable.sortMatch(en.p)
        } yield constructExpr(ENegBody(ENeg(sortedPar.term)), Node(Score.ENEG, sortedPar.score))
      case EVarBody(ev) =>
        for {
          sortedVar <- Sortable.sortMatch(ev.v)
        } yield constructExpr(EVarBody(EVar(sortedVar.term)), Node(Score.EVAR, sortedVar.score))
      case ENotBody(en) =>
        for {
          sortedPar <- Sortable.sortMatch(en.p)
        } yield constructExpr(ENotBody(ENot(sortedPar.term)), Node(Score.ENOT, sortedPar.score))
      case EMultBody(em) =>
        for {
          sortedPar1 <- Sortable.sortMatch(em.p1)
          sortedPar2 <- Sortable.sortMatch(em.p2)
        } yield
          constructExpr(
            EMultBody(EMult(sortedPar1.term, sortedPar2.term)),
            Node(Score.EMULT, sortedPar1.score, sortedPar2.score)
          )
      case EDivBody(ed) =>
        for {
          sortedPar1 <- Sortable.sortMatch(ed.p1)
          sortedPar2 <- Sortable.sortMatch(ed.p2)
        } yield
          constructExpr(
            EDivBody(EDiv(sortedPar1.term, sortedPar2.term)),
            Node(Score.EDIV, sortedPar1.score, sortedPar2.score)
          )
      case EPlusBody(ep) =>
        for {
          sortedPar1 <- Sortable.sortMatch(ep.p1)
          sortedPar2 <- Sortable.sortMatch(ep.p2)
        } yield
          constructExpr(
            EPlusBody(EPlus(sortedPar1.term, sortedPar2.term)),
            Node(Score.EPLUS, sortedPar1.score, sortedPar2.score)
          )
      case EMinusBody(em) =>
        for {
          sortedPar1 <- Sortable.sortMatch(em.p1)
          sortedPar2 <- Sortable.sortMatch(em.p2)
        } yield
          constructExpr(
            EMinusBody(EMinus(sortedPar1.term, sortedPar2.term)),
            Node(Score.EMINUS, sortedPar1.score, sortedPar2.score)
          )
      case ELtBody(el) =>
        for {
          sortedPar1 <- Sortable.sortMatch(el.p1)
          sortedPar2 <- Sortable.sortMatch(el.p2)
        } yield
          constructExpr(
            ELtBody(ELt(sortedPar1.term, sortedPar2.term)),
            Node(Score.ELT, sortedPar1.score, sortedPar2.score)
          )
      case ELteBody(el) =>
        for {
          sortedPar1 <- Sortable.sortMatch(el.p1)
          sortedPar2 <- Sortable.sortMatch(el.p2)
        } yield
          constructExpr(
            ELteBody(ELte(sortedPar1.term, sortedPar2.term)),
            Node(Score.ELTE, sortedPar1.score, sortedPar2.score)
          )
      case EGtBody(eg) =>
        for {
          sortedPar1 <- Sortable.sortMatch(eg.p1)
          sortedPar2 <- Sortable.sortMatch(eg.p2)
        } yield
          constructExpr(
            EGtBody(EGt(sortedPar1.term, sortedPar2.term)),
            Node(Score.EGT, sortedPar1.score, sortedPar2.score)
          )
      case EGteBody(eg) =>
        for {
          sortedPar1 <- Sortable.sortMatch(eg.p1)
          sortedPar2 <- Sortable.sortMatch(eg.p2)
        } yield
          constructExpr(
            EGteBody(EGte(sortedPar1.term, sortedPar2.term)),
            Node(Score.EGTE, sortedPar1.score, sortedPar2.score)
          )
      case EEqBody(ee) =>
        for {
          sortedPar1 <- Sortable.sortMatch(ee.p1)
          sortedPar2 <- Sortable.sortMatch(ee.p2)
        } yield
          constructExpr(
            EEqBody(EEq(sortedPar1.term, sortedPar2.term)),
            Node(Score.EEQ, sortedPar1.score, sortedPar2.score)
          )
      case ENeqBody(en) =>
        for {
          sortedPar1 <- Sortable.sortMatch(en.p1)
          sortedPar2 <- Sortable.sortMatch(en.p2)
        } yield
          constructExpr(
            ENeqBody(ENeq(sortedPar1.term, sortedPar2.term)),
            Node(Score.ENEQ, sortedPar1.score, sortedPar2.score)
          )
      case EAndBody(ea) =>
        for {
          sortedPar1 <- Sortable.sortMatch(ea.p1)
          sortedPar2 <- Sortable.sortMatch(ea.p2)
        } yield
          constructExpr(
            EAndBody(EAnd(sortedPar1.term, sortedPar2.term)),
            Node(Score.EAND, sortedPar1.score, sortedPar2.score)
          )
      case EOrBody(eo) =>
        for {
          sortedPar1 <- Sortable.sortMatch(eo.p1)
          sortedPar2 <- Sortable.sortMatch(eo.p2)
        } yield
          constructExpr(
            EOrBody(EOr(sortedPar1.term, sortedPar2.term)),
            Node(Score.EOR, sortedPar1.score, sortedPar2.score)
          )

      case EMatchesBody(em) =>
        for {
          sortedTarget  <- Sortable.sortMatch(em.target)
          sortedPattern <- Sortable.sortMatch(em.pattern)
        } yield
          constructExpr(
            EMatchesBody(EMatches(sortedTarget.term, sortedPattern.term)),
            Node(Score.EMATCHES, sortedTarget.score, sortedPattern.score)
          )
      case EPercentPercentBody(ep) =>
        for {
          sortedPar1 <- Sortable.sortMatch(ep.p1)
          sortedPar2 <- Sortable.sortMatch(ep.p2)
        } yield
          constructExpr(
            EPercentPercentBody(EPercentPercent(sortedPar1.term, sortedPar2.term)),
            Node(Score.EPERCENT, sortedPar1.score, sortedPar2.score)
          )
      case EPlusPlusBody(ep) =>
        for {
          sortedPar1 <- Sortable.sortMatch(ep.p1)
          sortedPar2 <- Sortable.sortMatch(ep.p2)
        } yield
          constructExpr(
            EPlusPlusBody(EPlusPlus(sortedPar1.term, sortedPar2.term)),
            Node(Score.EPLUSPLUS, sortedPar1.score, sortedPar2.score)
          )
      case EMinusMinusBody(ep) =>
        for {
          sortedPar1 <- Sortable.sortMatch(ep.p1)
          sortedPar2 <- Sortable.sortMatch(ep.p2)
        } yield
          constructExpr(
            EMinusMinusBody(EMinusMinus(sortedPar1.term, sortedPar2.term)),
            Node(Score.EMINUSMINUS, sortedPar1.score, sortedPar2.score)
          )
      case EMethodBody(em) =>
        for {
          args         <- em.arguments.toList.traverse(Sortable[Par].sortMatch[F])
          sortedTarget <- Sortable.sortMatch(em.target)
        } yield
          constructExpr(
            EMethodBody(em.withArguments(args.map(_.term.get)).withTarget(sortedTarget.term.get)),
            Node(
              Seq(Leaf(Score.EMETHOD), Leaf(em.methodName), sortedTarget.score) ++ args.map(_.score)
            )
          )
      case eg =>
        for {
          sortedGround <- Sortable.sortMatch(eg)
        } yield constructExpr(sortedGround.term, sortedGround.score)
    }
  }
}
