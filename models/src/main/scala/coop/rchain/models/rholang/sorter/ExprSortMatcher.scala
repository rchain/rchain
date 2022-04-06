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

    def remainderScore(remainder: Option[Var]): F[Tree[ScoreAtom]] =
      remainder match {
        case Some(_var) => Sortable[Var].sortMatch[F](_var).map(_.score)
        case None       => Sync[F].pure(Leaf(-1))
      }

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
        } yield constructExpr(
          EMultBody(EMult(sortedPar1.term, sortedPar2.term)),
          Node(Score.EMULT, sortedPar1.score, sortedPar2.score)
        )
      case EDivBody(ed) =>
        for {
          sortedPar1 <- Sortable.sortMatch(ed.p1)
          sortedPar2 <- Sortable.sortMatch(ed.p2)
        } yield constructExpr(
          EDivBody(EDiv(sortedPar1.term, sortedPar2.term)),
          Node(Score.EDIV, sortedPar1.score, sortedPar2.score)
        )
      case EModBody(ed) =>
        for {
          sortedPar1 <- Sortable.sortMatch(ed.p1)
          sortedPar2 <- Sortable.sortMatch(ed.p2)
        } yield constructExpr(
          EModBody(EMod(sortedPar1.term, sortedPar2.term)),
          Node(Score.EMOD, sortedPar1.score, sortedPar2.score)
        )
      case EPlusBody(ep) =>
        for {
          sortedPar1 <- Sortable.sortMatch(ep.p1)
          sortedPar2 <- Sortable.sortMatch(ep.p2)
        } yield constructExpr(
          EPlusBody(EPlus(sortedPar1.term, sortedPar2.term)),
          Node(Score.EPLUS, sortedPar1.score, sortedPar2.score)
        )
      case EMinusBody(em) =>
        for {
          sortedPar1 <- Sortable.sortMatch(em.p1)
          sortedPar2 <- Sortable.sortMatch(em.p2)
        } yield constructExpr(
          EMinusBody(EMinus(sortedPar1.term, sortedPar2.term)),
          Node(Score.EMINUS, sortedPar1.score, sortedPar2.score)
        )
      case ELtBody(el) =>
        for {
          sortedPar1 <- Sortable.sortMatch(el.p1)
          sortedPar2 <- Sortable.sortMatch(el.p2)
        } yield constructExpr(
          ELtBody(ELt(sortedPar1.term, sortedPar2.term)),
          Node(Score.ELT, sortedPar1.score, sortedPar2.score)
        )
      case ELteBody(el) =>
        for {
          sortedPar1 <- Sortable.sortMatch(el.p1)
          sortedPar2 <- Sortable.sortMatch(el.p2)
        } yield constructExpr(
          ELteBody(ELte(sortedPar1.term, sortedPar2.term)),
          Node(Score.ELTE, sortedPar1.score, sortedPar2.score)
        )
      case EGtBody(eg) =>
        for {
          sortedPar1 <- Sortable.sortMatch(eg.p1)
          sortedPar2 <- Sortable.sortMatch(eg.p2)
        } yield constructExpr(
          EGtBody(EGt(sortedPar1.term, sortedPar2.term)),
          Node(Score.EGT, sortedPar1.score, sortedPar2.score)
        )
      case EGteBody(eg) =>
        for {
          sortedPar1 <- Sortable.sortMatch(eg.p1)
          sortedPar2 <- Sortable.sortMatch(eg.p2)
        } yield constructExpr(
          EGteBody(EGte(sortedPar1.term, sortedPar2.term)),
          Node(Score.EGTE, sortedPar1.score, sortedPar2.score)
        )
      case EEqBody(ee) =>
        for {
          sortedPar1 <- Sortable.sortMatch(ee.p1)
          sortedPar2 <- Sortable.sortMatch(ee.p2)
        } yield constructExpr(
          EEqBody(EEq(sortedPar1.term, sortedPar2.term)),
          Node(Score.EEQ, sortedPar1.score, sortedPar2.score)
        )
      case ENeqBody(en) =>
        for {
          sortedPar1 <- Sortable.sortMatch(en.p1)
          sortedPar2 <- Sortable.sortMatch(en.p2)
        } yield constructExpr(
          ENeqBody(ENeq(sortedPar1.term, sortedPar2.term)),
          Node(Score.ENEQ, sortedPar1.score, sortedPar2.score)
        )
      case EAndBody(ea) =>
        for {
          sortedPar1 <- Sortable.sortMatch(ea.p1)
          sortedPar2 <- Sortable.sortMatch(ea.p2)
        } yield constructExpr(
          EAndBody(EAnd(sortedPar1.term, sortedPar2.term)),
          Node(Score.EAND, sortedPar1.score, sortedPar2.score)
        )
      case EOrBody(eo) =>
        for {
          sortedPar1 <- Sortable.sortMatch(eo.p1)
          sortedPar2 <- Sortable.sortMatch(eo.p2)
        } yield constructExpr(
          EOrBody(EOr(sortedPar1.term, sortedPar2.term)),
          Node(Score.EOR, sortedPar1.score, sortedPar2.score)
        )

      case EShortAndBody(ea) =>
        for {
          sortedPar1 <- Sortable.sortMatch(ea.p1)
          sortedPar2 <- Sortable.sortMatch(ea.p2)
        } yield constructExpr(
          EShortAndBody(EShortAnd(sortedPar1.term, sortedPar2.term)),
          Node(Score.ESHORTAND, sortedPar1.score, sortedPar2.score)
        )
      case EShortOrBody(eo) =>
        for {
          sortedPar1 <- Sortable.sortMatch(eo.p1)
          sortedPar2 <- Sortable.sortMatch(eo.p2)
        } yield constructExpr(
          EShortOrBody(EShortOr(sortedPar1.term, sortedPar2.term)),
          Node(Score.ESHORTOR, sortedPar1.score, sortedPar2.score)
        )

      case EMatchesBody(em) =>
        for {
          sortedTarget  <- Sortable.sortMatch(em.target)
          sortedPattern <- Sortable.sortMatch(em.pattern)
        } yield constructExpr(
          EMatchesBody(EMatches(sortedTarget.term, sortedPattern.term)),
          Node(Score.EMATCHES, sortedTarget.score, sortedPattern.score)
        )
      case EPercentPercentBody(ep) =>
        for {
          sortedPar1 <- Sortable.sortMatch(ep.p1)
          sortedPar2 <- Sortable.sortMatch(ep.p2)
        } yield constructExpr(
          EPercentPercentBody(EPercentPercent(sortedPar1.term, sortedPar2.term)),
          Node(Score.EPERCENT, sortedPar1.score, sortedPar2.score)
        )
      case EPlusPlusBody(ep) =>
        for {
          sortedPar1 <- Sortable.sortMatch(ep.p1)
          sortedPar2 <- Sortable.sortMatch(ep.p2)
        } yield constructExpr(
          EPlusPlusBody(EPlusPlus(sortedPar1.term, sortedPar2.term)),
          Node(Score.EPLUSPLUS, sortedPar1.score, sortedPar2.score)
        )
      case EMinusMinusBody(ep) =>
        for {
          sortedPar1 <- Sortable.sortMatch(ep.p1)
          sortedPar2 <- Sortable.sortMatch(ep.p2)
        } yield constructExpr(
          EMinusMinusBody(EMinusMinus(sortedPar1.term, sortedPar2.term)),
          Node(Score.EMINUSMINUS, sortedPar1.score, sortedPar2.score)
        )
      case EMapBody(parMap) =>
        def sortKeyValuePair(key: Par, value: Par): F[ScoredTerm[(Par, Par)]] =
          for {
            sortedKey   <- Sortable.sortMatch(key)
            sortedValue <- Sortable.sortMatch(value)
          } yield ScoredTerm((sortedKey.term, sortedValue.term), sortedKey.score)

        for {
          sortedPars          <- parMap.ps.sortedList.traverse(kv => sortKeyValuePair(kv._1, kv._2))
          remainderScore      <- remainderScore(parMap.remainder)
          connectiveUsedScore = if (parMap.connectiveUsed) 1L else 0L
        } yield constructExpr(
          EMapBody(
            ParMap(
              sortedPars.map(_.term),
              parMap.connectiveUsed,
              parMap.locallyFree,
              parMap.remainder
            )
          ),
          Node(
            Seq(Leaf(Score.EMAP), remainderScore) ++ sortedPars.map(_.score) ++ Seq(
              Leaf(connectiveUsedScore)
            )
          )
        )
      case ESetBody(parSet) =>
        for {
          sortedPars          <- parSet.ps.sortedPars.traverse(Sortable[Par].sortMatch[F])
          remainderScore      <- remainderScore(parSet.remainder)
          connectiveUsedScore = if (parSet.connectiveUsed) 1L else 0L
        } yield constructExpr(
          ESetBody(
            ParSet(
              SortedParHashSet(sortedPars.map(_.term)),
              parSet.connectiveUsed,
              parSet.locallyFree,
              parSet.remainder
            )
          ),
          Node(
            Seq(Leaf(Score.ESET), remainderScore) ++ sortedPars
              .map(_.score) ++ Seq(Leaf(connectiveUsedScore))
          )
        )
      case EListBody(list) =>
        for {
          pars                <- list.ps.toList.traverse(Sortable[Par].sortMatch[F])
          remainderScore      <- remainderScore(list.remainder)
          connectiveUsedScore = if (list.connectiveUsed) 1L else 0L
        } yield constructExpr(
          EListBody(
            EList(pars.map(_.term), list.locallyFree, list.connectiveUsed, list.remainder)
          ),
          Node(
            Seq(Leaf(Score.ELIST), remainderScore) ++ pars.map(_.score) ++ Seq(
              Leaf(connectiveUsedScore)
            )
          )
        )
      case ETupleBody(tuple) =>
        for {
          sortedPars          <- tuple.ps.toList.traverse(Sortable[Par].sortMatch[F])
          connectiveUsedScore = if (tuple.connectiveUsed) 1L else 0L
        } yield ScoredTerm(
          ETupleBody(tuple.withPs(sortedPars.map(_.term))),
          Node(
            Seq(Leaf(Score.ETUPLE)) ++ sortedPars.map(_.score) ++ Seq(Leaf(connectiveUsedScore))
          )
        )
      case EMethodBody(em) =>
        for {
          args                <- em.arguments.toList.traverse(Sortable[Par].sortMatch[F])
          sortedTarget        <- Sortable.sortMatch(em.target)
          connectiveUsedScore = if (em.connectiveUsed) 1L else 0L
        } yield constructExpr(
          EMethodBody(em.withArguments(args.map(_.term)).withTarget(sortedTarget.term)),
          Node(
            Seq(Leaf(Score.EMETHOD), Leaf(em.methodName), sortedTarget.score) ++ args
              .map(_.score) ++ Seq(Leaf(connectiveUsedScore))
          )
        )
      case gb: GBool =>
        Sortable.sortMatch(gb).map { sorted =>
          ScoredTerm(e, sorted.score)
        }
      case gi: GInt    => ScoredTerm(e, Leaves(Score.INT, gi.value)).pure[F]
      case gs: GString => ScoredTerm(e, Node(Score.STRING, Leaf(gs.value))).pure[F]
      case gu: GUri    => ScoredTerm(e, Node(Score.URI, Leaf(gu.value))).pure[F]
      case GByteArray(ba) =>
        ScoredTerm(e, Node(Score.EBYTEARR, Leaf(ba.toStringUtf8))).pure[F]
      //TODO get rid of Empty nodes in Protobuf unless they represent sth indeed optional
      case Empty =>
        ScoredTerm(e, Node(Score.ABSENT)).pure[F]
      case expr => //TODO(mateusz.gorski): rethink it
        Sync[F].raiseError(
          new IllegalArgumentException(s"ExprSortMatcher passed unknown Expr instance:\n$expr")
        )
    }
  }
}
