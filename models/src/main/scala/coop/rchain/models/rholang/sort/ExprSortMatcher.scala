package coop.rchain.models.rholang.sort

import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._

private[sort] object ExprSortMatcher extends Sortable[Expr] {
  private def sortBinaryOperation(p1: Par, p2: Par): (ScoredTerm[Par], ScoredTerm[Par]) =
    (Sortable.sortMatch(p1), Sortable.sortMatch(p2))

  def sortMatch(e: Expr): ScoredTerm[Expr] = {
    def constructExpr(exprInstance: ExprInstance, score: Tree[ScoreAtom]) =
      ScoredTerm(Expr(exprInstance = exprInstance), score)

    e.exprInstance match {
      case ENegBody(en) =>
        val sortedPar = Sortable.sortMatch(en.p)
        constructExpr(ENegBody(ENeg(sortedPar.term)), Node(Score.ENEG, sortedPar.score))
      case EVarBody(ev) =>
        val sortedVar = Sortable.sortMatch(ev.v)
        constructExpr(EVarBody(EVar(sortedVar.term)), Node(Score.EVAR, sortedVar.score))
      case EEvalBody(chan) =>
        val sortedChan = Sortable.sortMatch(chan)
        constructExpr(EEvalBody(sortedChan.term), Node(Score.EEVAL, sortedChan.score))
      case ENotBody(en) =>
        val sortedPar = Sortable.sortMatch(en.p)
        constructExpr(ENotBody(ENot(sortedPar.term)), Node(Score.ENOT, sortedPar.score))
      case EMultBody(em) =>
        val (sortedPar1, sortedPar2) = sortBinaryOperation(em.p1, em.p2)
        constructExpr(EMultBody(EMult(sortedPar1.term, sortedPar2.term)),
                      Node(Score.EMULT, sortedPar1.score, sortedPar2.score))
      case EDivBody(ed) =>
        val (sortedPar1, sortedPar2) = sortBinaryOperation(ed.p1, ed.p2)
        constructExpr(EDivBody(EDiv(sortedPar1.term, sortedPar2.term)),
                      Node(Score.EDIV, sortedPar1.score, sortedPar2.score))
      case EPlusBody(ep) =>
        val (sortedPar1, sortedPar2) = sortBinaryOperation(ep.p1, ep.p2)
        constructExpr(EPlusBody(EPlus(sortedPar1.term, sortedPar2.term)),
                      Node(Score.EPLUS, sortedPar1.score, sortedPar2.score))
      case EMinusBody(em) =>
        val (sortedPar1, sortedPar2) = sortBinaryOperation(em.p1, em.p2)
        constructExpr(EMinusBody(EMinus(sortedPar1.term, sortedPar2.term)),
                      Node(Score.EMINUS, sortedPar1.score, sortedPar2.score))
      case ELtBody(el) =>
        val (sortedPar1, sortedPar2) = sortBinaryOperation(el.p1, el.p2)
        constructExpr(ELtBody(ELt(sortedPar1.term, sortedPar2.term)),
                      Node(Score.ELT, sortedPar1.score, sortedPar2.score))
      case ELteBody(el) =>
        val (sortedPar1, sortedPar2) = sortBinaryOperation(el.p1, el.p2)
        constructExpr(ELteBody(ELte(sortedPar1.term, sortedPar2.term)),
                      Node(Score.ELTE, sortedPar1.score, sortedPar2.score))
      case EGtBody(eg) =>
        val (sortedPar1, sortedPar2) = sortBinaryOperation(eg.p1, eg.p2)
        constructExpr(EGtBody(EGt(sortedPar1.term, sortedPar2.term)),
                      Node(Score.EGT, sortedPar1.score, sortedPar2.score))
      case EGteBody(eg) =>
        val (sortedPar1, sortedPar2) = sortBinaryOperation(eg.p1, eg.p2)
        constructExpr(EGteBody(EGte(sortedPar1.term, sortedPar2.term)),
                      Node(Score.EGTE, sortedPar1.score, sortedPar2.score))
      case EEqBody(ee) =>
        val (sortedPar1, sortedPar2) = sortBinaryOperation(ee.p1, ee.p2)
        constructExpr(EEqBody(EEq(sortedPar1.term, sortedPar2.term)),
                      Node(Score.EEQ, sortedPar1.score, sortedPar2.score))
      case ENeqBody(en) =>
        val (sortedPar1, sortedPar2) = sortBinaryOperation(en.p1, en.p2)
        constructExpr(ENeqBody(ENeq(sortedPar1.term, sortedPar2.term)),
                      Node(Score.ENEQ, sortedPar1.score, sortedPar2.score))
      case EAndBody(ea) =>
        val (sortedPar1, sortedPar2) = sortBinaryOperation(ea.p1, ea.p2)
        constructExpr(EAndBody(EAnd(sortedPar1.term, sortedPar2.term)),
                      Node(Score.EAND, sortedPar1.score, sortedPar2.score))
      case EOrBody(eo) =>
        val (sortedPar1, sortedPar2) = sortBinaryOperation(eo.p1, eo.p2)
        constructExpr(EOrBody(EOr(sortedPar1.term, sortedPar2.term)),
                      Node(Score.EOR, sortedPar1.score, sortedPar2.score))

      case EMatchesBody(em) =>
        val (sortedTarget, sortedPattern) = sortBinaryOperation(em.target, em.pattern)
        constructExpr(EMatchesBody(EMatches(sortedTarget.term, sortedPattern.term)),
                      Node(Score.EMATCHES, sortedTarget.score, sortedPattern.score))
      case EPercentPercentBody(ep) =>
        val (sortedPar1, sortedPar2) = sortBinaryOperation(ep.p1, ep.p2)
        constructExpr(EPercentPercentBody(EPercentPercent(sortedPar1.term, sortedPar2.term)),
                      Node(Score.EPERCENT, sortedPar1.score, sortedPar2.score))
      case EPlusPlusBody(ep) =>
        val (sortedPar1, sortedPar2) = sortBinaryOperation(ep.p1, ep.p2)
        constructExpr(EPlusPlusBody(EPlusPlus(sortedPar1.term, sortedPar2.term)),
                      Node(Score.EPLUSPLUS, sortedPar1.score, sortedPar2.score))
      case EMinusMinusBody(ep) =>
        val (sortedPar1, sortedPar2) = sortBinaryOperation(ep.p1, ep.p2)
        constructExpr(EMinusMinusBody(EMinusMinus(sortedPar1.term, sortedPar2.term)),
                      Node(Score.EMINUSMINUS, sortedPar1.score, sortedPar2.score))
      case EMethodBody(em) =>
        val args         = em.arguments.toList.map(par => Sortable.sortMatch(par))
        val sortedTarget = Sortable.sortMatch(em.target)
        constructExpr(
          EMethodBody(em.withArguments(args.map(_.term.get)).withTarget(sortedTarget.term.get)),
          Node(
            Seq(Leaf(Score.EMETHOD), Leaf(em.methodName), sortedTarget.score) ++ args.map(_.score))
        )
      case eg =>
        val sortedGround = Sortable.sortMatch(eg)
        constructExpr(sortedGround.term, sortedGround.score)
    }
  }
}
