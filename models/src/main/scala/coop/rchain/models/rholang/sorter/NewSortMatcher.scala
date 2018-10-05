package coop.rchain.models.rholang.sorter

import cats.effect.Sync
import coop.rchain.models.{Expr, New}
import cats.implicits._

private[sorter] object NewSortMatcher extends Sortable[New] {
  def sortMatch[F[_]: Sync](n: New): F[ScoredTerm[New]] =
    for {
      sortedPar <- Sortable.sortMatch(n.p)
    } yield
      ScoredTerm(
        New(bindCount = n.bindCount, p = sortedPar.term, uri = n.uri, locallyFree = n.locallyFree),
        new Node(Leaf(Score.NEW) +: (Leaf(n.bindCount) +: n.uri.map(Leaf.apply) :+ sortedPar.score))
      )
}
