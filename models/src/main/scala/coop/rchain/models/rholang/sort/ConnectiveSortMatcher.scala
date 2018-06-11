package coop.rchain.models.rholang.sort

import coop.rchain.models.Connective
import coop.rchain.models.Connective.ConnectiveInstance.{ConnAndBody, ConnNotBody, ConnOrBody}
import cats.implicits._
import coop.rchain.models.rholang.implicits._

object ConnectiveSortMatcher {
  def sortMatch(c: Connective): Either[Throwable, ScoredTerm[Connective]] =
    c.connectiveInstance match {
      case ConnAndBody(cb) =>
        cb.ps.toList
          .traverse(par => ParSortMatcher.sortMatch(par))
          .map(pars =>
            ScoredTerm(Connective(ConnAndBody(cb.withPs(pars.map(_.term.get)))),
                       Node(Score.CONNECTIVE_AND, pars.map(_.score): _*)))
      case ConnOrBody(cb) =>
        cb.ps.toList
          .traverse(par => ParSortMatcher.sortMatch(par))
          .map(pars =>
            ScoredTerm(Connective(ConnOrBody(cb.withPs(pars.map(_.term.get)))),
                       Node(Score.CONNECTIVE_OR, pars.map(_.score): _*)))
      case ConnNotBody(p) =>
        ParSortMatcher
          .sortMatch(p)
          .map(
            scoredPar =>
              ScoredTerm(Connective(ConnNotBody(scoredPar.term.get)),
                         Node(Score.CONNECTIVE_NOT, scoredPar.score)))
    }
}
