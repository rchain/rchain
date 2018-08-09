package coop.rchain.models.rholang.sort

import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.{Connective, VarRef}
import coop.rchain.models.rholang.implicits._

private[sort] object ConnectiveSortMatcher extends Sortable[Connective] {
  def sortMatch(c: Connective): ScoredTerm[Connective] =
    c.connectiveInstance match {
      case ConnAndBody(cb) =>
        val pars = cb.ps.toList.map(par => Sortable.sortMatch(par))
        ScoredTerm(Connective(ConnAndBody(cb.withPs(pars.map(_.term.get)))),
                   Node(Score.CONNECTIVE_AND, pars.map(_.score): _*))
      case ConnOrBody(cb) =>
        val pars = cb.ps.toList.map(par => Sortable.sortMatch(par))
        ScoredTerm(Connective(ConnOrBody(cb.withPs(pars.map(_.term.get)))),
                   Node(Score.CONNECTIVE_OR, pars.map(_.score): _*))
      case ConnNotBody(p) =>
        val scoredPar = Sortable.sortMatch(p)
        ScoredTerm(Connective(ConnNotBody(scoredPar.term.get)),
                   Node(Score.CONNECTIVE_NOT, scoredPar.score))
      case v @ VarRefBody(VarRef(index, depth)) =>
        ScoredTerm(Connective(v), Leaves(Score.CONNECTIVE_VARREF, index, depth))
      case v @ ConnBool(_) =>
        ScoredTerm(Connective(v), Leaf(Score.CONNECTIVE_BOOL))
      case v @ ConnInt(_) =>
        ScoredTerm(Connective(v), Leaf(Score.CONNECTIVE_INT))
      case v @ ConnString(_) =>
        ScoredTerm(Connective(v), Leaf(Score.CONNECTIVE_STRING))
      case v @ ConnUri(_) =>
        ScoredTerm(Connective(v), Leaf(Score.CONNECTIVE_URI))
      case v @ ConnByteArray(_) =>
        ScoredTerm(Connective(v), Leaf(Score.CONNECTIVE_BYTEARRAY))
      case Empty =>
        ScoredTerm(Connective(Empty), Leaf(Score.ABSENT))
    }
}
