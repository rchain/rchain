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
        } yield ScoredTerm(
          Connective(ConnAndBody(cb.withPs(pars.map(_.term)))),
          Node(Score.CONNECTIVE_AND, pars.map(_.score): _*)
        )
      case ConnOrBody(cb) =>
        for {
          pars <- cb.ps.toList.traverse(Sortable[Par].sortMatch[F])
        } yield ScoredTerm(
          Connective(ConnOrBody(cb.withPs(pars.map(_.term)))),
          Node(Score.CONNECTIVE_OR, pars.map(_.score): _*)
        )
      case ConnNotBody(p) =>
        for {
          scoredPar <- Sortable.sortMatch(p)
        } yield ScoredTerm(
          Connective(ConnNotBody(scoredPar.term)),
          Node(Score.CONNECTIVE_NOT, scoredPar.score)
        )
      case v @ VarRefBody(VarRef(index, depth)) =>
        ScoredTerm(Connective(v), Leaves(Score.CONNECTIVE_VARREF, index.toLong, depth.toLong))
          .pure[F]
      case v @ ConnBool(b) =>
        ScoredTerm(Connective(v), Leaves(Score.CONNECTIVE_BOOL, if (b) 1 else 0)).pure[F]
      case v @ ConnInt(b) =>
        ScoredTerm(Connective(v), Leaves(Score.CONNECTIVE_INT, if (b) 1 else 0)).pure[F]
      case v @ ConnBigInt(b) =>
        ScoredTerm(Connective(v), Leaves(Score.CONNECTIVE_BIG_INT, if (b) 1 else 0)).pure[F]
      case v @ ConnString(b) =>
        ScoredTerm(Connective(v), Leaves(Score.CONNECTIVE_STRING, if (b) 1 else 0)).pure[F]
      case v @ ConnUri(b) =>
        ScoredTerm(Connective(v), Leaves(Score.CONNECTIVE_URI, if (b) 1 else 0)).pure[F]
      case v @ ConnByteArray(b) =>
        ScoredTerm(Connective(v), Leaves(Score.CONNECTIVE_BYTEARRAY, if (b) 1 else 0)).pure[F]
      case Empty =>
        ScoredTerm(Connective(Empty), Leaf(Score.ABSENT)).pure[F]
    }
}
