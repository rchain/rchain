package coop.rchain.models.rholang.sort

import coop.rchain.models.Connective
import coop.rchain.models.Connective.ConnectiveInstance.{
  ConnAndBody,
  ConnNotBody,
  ConnOrBody,
  Empty,
  VarRefBody
}
import coop.rchain.models.VarRef
import cats.implicits._
import coop.rchain.models.rholang.implicits._

object ConnectiveSortMatcher extends Sortable[Connective] {
  def sortMatch(c: Connective): ScoredTerm[Connective] =
    c.connectiveInstance match {
      case ConnAndBody(cb) =>
        val pars = cb.ps.toList.map(par => ParSortMatcher.sortMatch(par))
        ScoredTerm(Connective(ConnAndBody(cb.withPs(pars.map(_.term.get)))),
                   Node(Score.CONNECTIVE_AND, pars.map(_.score): _*))
      case ConnOrBody(cb) =>
        val pars = cb.ps.toList.map(par => ParSortMatcher.sortMatch(par))
        ScoredTerm(Connective(ConnOrBody(cb.withPs(pars.map(_.term.get)))),
                   Node(Score.CONNECTIVE_OR, pars.map(_.score): _*))
      case ConnNotBody(p) =>
        val scoredPar = ParSortMatcher.sortMatch(p)
        ScoredTerm(Connective(ConnNotBody(scoredPar.term.get)),
                   Node(Score.CONNECTIVE_NOT, scoredPar.score))
      case v @ VarRefBody(VarRef(index, depth)) =>
        ScoredTerm(Connective(v), Leaves(Score.CONNECTIVE_VARREF, index, depth))
      case Empty =>
        ScoredTerm(Connective(Empty), Leaf(Score.ABSENT))
    }
}
