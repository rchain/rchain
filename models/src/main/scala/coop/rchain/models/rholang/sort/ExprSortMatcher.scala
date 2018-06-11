package coop.rchain.models.rholang.sort

import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import cats.implicits._

object ExprSortMatcher {
  private def sortBinaryOperation(
      p1: Option[Par],
      p2: Option[Par]): Either[Throwable, (ScoredTerm[Par], ScoredTerm[Par])] =
    for {
      p1 <- ParSortMatcher.sortMatch(p1)
      p2 <- ParSortMatcher.sortMatch(p2)
    } yield (p1, p2)

  def sortMatch(e: Expr): Either[Throwable, ScoredTerm[Expr]] = {
    def constructExpr(exprInstance: ExprInstance, score: Tree[ScoreAtom]) =
      ScoredTerm(Expr(exprInstance = exprInstance), score)
    e.exprInstance match {
      case ENegBody(en) =>
        ParSortMatcher
          .sortMatch(en.p)
          .map(sortedPar =>
            constructExpr(ENegBody(ENeg(sortedPar.term)), Node(Score.ENEG, sortedPar.score)))
      case EVarBody(ev) =>
        VarSortMatcher
          .sortMatch(ev.v)
          .map(sortedVar =>
            constructExpr(EVarBody(EVar(sortedVar.term)), Node(Score.EVAR, sortedVar.score)))
      case EEvalBody(chan) =>
        ChannelSortMatcher
          .sortMatch(chan)
          .map(sortedChan =>
            constructExpr(EEvalBody(sortedChan.term), Node(Score.EEVAL, sortedChan.score)))
      case ENotBody(en) =>
        ParSortMatcher
          .sortMatch(en.p)
          .map(sortedPar =>
            constructExpr(ENotBody(ENot(sortedPar.term)), Node(Score.ENOT, sortedPar.score)))
      case EMultBody(em) =>
        sortBinaryOperation(em.p1, em.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(EMultBody(EMult(sortedPar1.term, sortedPar2.term)),
                          Node(Score.EMULT, sortedPar1.score, sortedPar2.score))
        }
      case EDivBody(ed) =>
        sortBinaryOperation(ed.p1, ed.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(EDivBody(EDiv(sortedPar1.term, sortedPar2.term)),
                          Node(Score.EDIV, sortedPar1.score, sortedPar2.score))
        }
      case EPlusBody(ep) =>
        sortBinaryOperation(ep.p1, ep.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(EPlusBody(EPlus(sortedPar1.term, sortedPar2.term)),
                          Node(Score.EPLUS, sortedPar1.score, sortedPar2.score))
        }
      case EMinusBody(em) =>
        sortBinaryOperation(em.p1, em.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(EMinusBody(EMinus(sortedPar1.term, sortedPar2.term)),
                          Node(Score.EMINUS, sortedPar1.score, sortedPar2.score))
        }
      case ELtBody(el) =>
        sortBinaryOperation(el.p1, el.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(ELtBody(ELt(sortedPar1.term, sortedPar2.term)),
                          Node(Score.ELT, sortedPar1.score, sortedPar2.score))
        }
      case ELteBody(el) =>
        sortBinaryOperation(el.p1, el.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(ELteBody(ELte(sortedPar1.term, sortedPar2.term)),
                          Node(Score.ELTE, sortedPar1.score, sortedPar2.score))
        }
      case EGtBody(eg) =>
        sortBinaryOperation(eg.p1, eg.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(EGtBody(EGt(sortedPar1.term, sortedPar2.term)),
                          Node(Score.EGT, sortedPar1.score, sortedPar2.score))
        }
      case EGteBody(eg) =>
        sortBinaryOperation(eg.p1, eg.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(EGteBody(EGte(sortedPar1.term, sortedPar2.term)),
                          Node(Score.EGTE, sortedPar1.score, sortedPar2.score))
        }
      case EEqBody(ee) =>
        sortBinaryOperation(ee.p1, ee.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(EEqBody(EEq(sortedPar1.term, sortedPar2.term)),
                          Node(Score.EEQ, sortedPar1.score, sortedPar2.score))
        }
      case ENeqBody(en) =>
        sortBinaryOperation(en.p1, en.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(ENeqBody(ENeq(sortedPar1.term, sortedPar2.term)),
                          Node(Score.ENEQ, sortedPar1.score, sortedPar2.score))
        }
      case EAndBody(ea) =>
        sortBinaryOperation(ea.p1, ea.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(EAndBody(EAnd(sortedPar1.term, sortedPar2.term)),
                          Node(Score.EAND, sortedPar1.score, sortedPar2.score))
        }
      case EOrBody(eo) =>
        sortBinaryOperation(eo.p1, eo.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(EOrBody(EOr(sortedPar1.term, sortedPar2.term)),
                          Node(Score.EOR, sortedPar1.score, sortedPar2.score))
        }
      case EMethodBody(em) =>
        for {
          args         <- em.arguments.toList.traverse(par => ParSortMatcher.sortMatch(par))
          sortedTarget <- ParSortMatcher.sortMatch(em.target.get)
        } yield
          constructExpr(
            EMethodBody(em.withArguments(args.map(_.term.get)).withTarget(sortedTarget.term.get)),
            Node(
              Seq(Leaf(Score.EMETHOD), Leaf(em.methodName), sortedTarget.score) ++ args.map(
                _.score))
          )
      case eg =>
        GroundSortMatcher
          .sortMatch(eg)
          .map(sortedGround => constructExpr(sortedGround.term, sortedGround.score))
    }
  }
}
