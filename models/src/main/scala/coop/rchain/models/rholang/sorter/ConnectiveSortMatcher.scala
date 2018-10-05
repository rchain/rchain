package coop.rchain.models.rholang.sorter

import cats.effect.Sync
import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.{Connective, Par, VarRef}
import coop.rchain.models.rholang.implicits._
import cats.implicits._

private[sorter] object ConnectiveSortMatcher extends Sortable[Connective] {
  def sortMatch[F[_]: Sync](c: Connective): F[ScoredTerm[Connective]] =
    c.connectiveInstance match {
      case ConnAndBody(cb) =>
        for {
          pars <- cb.ps.toList.traverse(Sortable[Par].sortMatch[F])
        } yield
          ScoredTerm(
            Connective(ConnAndBody(cb.withPs(pars.map(_.term.get)))),
            Node(Score.CONNECTIVE_AND, pars.map(_.score): _*)
          )
      case ConnOrBody(cb) =>
        for {
          pars <- cb.ps.toList.traverse(Sortable[Par].sortMatch[F])
        } yield
          ScoredTerm(
            Connective(ConnOrBody(cb.withPs(pars.map(_.term.get)))),
            Node(Score.CONNECTIVE_OR, pars.map(_.score): _*)
          )
      case ConnNotBody(p) =>
        for {
          scoredPar <- Sortable.sortMatch(p)
        } yield
          ScoredTerm(
            Connective(ConnNotBody(scoredPar.term.get)),
            Node(Score.CONNECTIVE_NOT, scoredPar.score)
          )
      case v @ VarRefBody(VarRef(index, depth)) =>
        ScoredTerm(Connective(v), Leaves(Score.CONNECTIVE_VARREF, index, depth)).pure[F]
      case v @ ConnBool(_) =>
        ScoredTerm(Connective(v), Leaf(Score.CONNECTIVE_BOOL)).pure[F]
      case v @ ConnInt(_) =>
        ScoredTerm(Connective(v), Leaf(Score.CONNECTIVE_INT)).pure[F]
      case v @ ConnString(_) =>
        ScoredTerm(Connective(v), Leaf(Score.CONNECTIVE_STRING)).pure[F]
      case v @ ConnUri(_) =>
        ScoredTerm(Connective(v), Leaf(Score.CONNECTIVE_URI)).pure[F]
      case v @ ConnByteArray(_) =>
        ScoredTerm(Connective(v), Leaf(Score.CONNECTIVE_BYTEARRAY)).pure[F]
      case Empty =>
        ScoredTerm(Connective(Empty), Leaf(Score.ABSENT)).pure[F]
    }
}
